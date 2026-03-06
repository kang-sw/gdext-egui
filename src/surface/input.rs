use godot::{
    classes::{
        self, DisplayServer, InputEventKey, InputEventMouse, InputEventMouseButton,
        InputEventMouseMotion,
    },
    global::{self, KeyModifierMask},
    prelude::*,
};
use tap::prelude::Tap;

use crate::helpers::ToCounterpart;

use super::EguiViewportBridge;

impl EguiViewportBridge {
    /// Try to consume the input once, and returns whether the input was consumed or not.
    ///
    /// NOTE: This was separated from virtual `input` method, to make `tool` input
    /// handling available.
    pub fn try_consume_input(&mut self, event: Gd<classes::InputEvent>) -> bool {
        let Some(ctx) = &self.context else {
            return false;
        };

        let ui_scale = self.ui_scale_cache;
        let pos_offset = self.base().get_global_position().to_counterpart();
        let calc_mouse_pos =
            |ev: &InputEventMouse| (ev.get_position().to_alternative() - pos_offset) / ui_scale;

        let event = match event.try_cast::<InputEventMouseMotion>() {
            Err(event) => event,
            Ok(event) => {
                self.on_event(egui::Event::PointerMoved(calc_mouse_pos(
                    event.upcast_ref(),
                )));

                return ctx.is_pointer_over_area();
            }
        };

        let event = match event.try_cast::<InputEventMouseButton>() {
            Err(event) => event,
            Ok(event) => {
                if event.is_canceled() {
                    return false;
                }

                let modifiers = modifier_to_egui(event.get_modifiers_mask());
                let button = event.get_button_index();
                let pos = calc_mouse_pos(event.upcast_ref());

                #[derive(Debug)]
                enum Type {
                    Btn(egui::PointerButton),
                    Wheel { horizontal: bool, delta: f32 },
                }

                use Type::*;

                let factor = event.get_factor().tap_mut(|val| {
                    if *val == 0.0 {
                        *val = 4.0;
                    }
                });

                let r = match button {
                    global::MouseButton::LEFT => Btn(egui::PointerButton::Primary),
                    global::MouseButton::RIGHT => Btn(egui::PointerButton::Secondary),
                    global::MouseButton::MIDDLE => Btn(egui::PointerButton::Middle),
                    global::MouseButton::XBUTTON1 => Btn(egui::PointerButton::Extra1),
                    global::MouseButton::XBUTTON2 => Btn(egui::PointerButton::Extra2),

                    global::MouseButton::WHEEL_DOWN => Wheel {
                        horizontal: false,
                        delta: -factor,
                    },
                    global::MouseButton::WHEEL_UP => Wheel {
                        horizontal: false,
                        delta: factor,
                    },
                    global::MouseButton::WHEEL_RIGHT => Wheel {
                        horizontal: true,
                        delta: factor,
                    },
                    global::MouseButton::WHEEL_LEFT => Wheel {
                        horizontal: true,
                        delta: -factor,
                    },
                    _ => return false,
                };

                match r {
                    Btn(button) => self.on_event(egui::Event::PointerButton {
                        pos,
                        button,
                        pressed: event.is_pressed(),
                        modifiers,
                    }),
                    Wheel { horizontal, delta } => {
                        self.on_event(egui::Event::MouseWheel {
                            unit: egui::MouseWheelUnit::Line,
                            delta: egui::vec2(
                                if horizontal { delta } else { 0.0 },
                                if !horizontal { delta } else { 0.0 },
                            ),
                            modifiers,
                        });

                        // Check if it's scrolling
                        if modifiers.matches_logically(egui::Modifiers::CTRL) {
                            // Zoom value should be in range -1 ~ 1 => B^(powf)
                            const B: f32 = 2.0;
                            self.on_event(egui::Event::Zoom(B.powf(delta)));
                        } else {
                            #[cfg(any())]
                            // egui::Event::Scroll has disappeared ... seems replaced by MouseWheel event.
                            {
                                let delta = if modifiers.shift_only() {
                                    // Horizontal
                                    Some([delta, 0.0])
                                } else if modifiers.is_none() {
                                    Some([0.0, delta])
                                } else {
                                    None
                                };

                                if let Some(_delta) = delta {
                                    const SCROLL_AMOUNT: f32 = 100.;

                                    // TODO: in 0.30, `Scroll` event seems removed. Check if sending
                                    // `MouseWheel` event is sufficient.

                                    self.on_event(egui::Event::MouseWheel(
                                        egui::Vec2::from(delta) * SCROLL_AMOUNT,
                                    ));
                                }
                            }
                        }
                    }
                };

                return if ctx.wants_pointer_input() {
                    // We grab focus only with clicks
                    self.base_mut().grab_focus();
                    true
                } else {
                    false
                };
            }
        };

        let event = match event.try_cast::<InputEventKey>() {
            Err(event) => event,
            Ok(event) => {
                let key = event.get_keycode();
                let modifiers = modifier_to_egui(event.get_modifiers_mask());

                // Handle copy / cut / paste ...
                if modifiers.matches_logically(egui::Modifiers::CTRL) {
                    let event = match key {
                        global::Key::C => Some(egui::Event::Copy),
                        global::Key::X => Some(egui::Event::Cut),
                        global::Key::V => Some(egui::Event::Paste(
                            DisplayServer::singleton().clipboard_get().into(),
                        )),
                        _ => None,
                    };

                    if let Some(event) = event {
                        self.on_event(event);
                        return ctx.wants_keyboard_input();
                    }
                }

                // @See
                // https://github.com/godotengine/godot/blob/16d61427cab3a8e43f0a9a8ee724fc176b6433c6/scene/gui/text_edit.cpp#L2267
                let unicode = event.get_unicode();

                if event.is_pressed() && unicode >= 32 {
                    let ch = std::char::from_u32(unicode).unwrap();
                    self.on_event(egui::Event::Text(ch.to_string()));
                }

                let event = key_to_egui(key).map(|key| egui::Event::Key {
                    key,
                    physical_key: key_to_egui(event.get_keycode()),
                    pressed: event.is_pressed(),
                    repeat: event.is_echo(),
                    modifiers,
                });

                if let Some(event) = event {
                    self.on_event(event);
                }

                return ctx.wants_keyboard_input();
            }
        };

        // Every event catch was exhausted. Simply ignore it!
        let _ = event;

        false
    }
}

pub(crate) fn modifier_to_egui(modifier: KeyModifierMask) -> egui::Modifiers {
    let mut out = egui::Modifiers::default();

    let has = |x: KeyModifierMask| modifier.ord() & x.ord() != 0;

    out.shift = has(KeyModifierMask::SHIFT);
    out.ctrl = has(KeyModifierMask::CTRL | KeyModifierMask::CMD_OR_CTRL);
    out.command = has(KeyModifierMask::CTRL | KeyModifierMask::CMD_OR_CTRL);
    out.mac_cmd = has(KeyModifierMask::CMD_OR_CTRL);
    out.alt = has(KeyModifierMask::ALT);

    out
}

pub(crate) fn key_to_egui(key: global::Key) -> Option<egui::Key> {
    match key {
        // global::Key::NONE => egui::Key::None,
        // global::Key::SPECIAL => egui::Key::Special,
        global::Key::ESCAPE => egui::Key::Escape,
        global::Key::TAB => egui::Key::Tab,
        // global::Key::BACKTAB => egui::Key::Backtab,
        global::Key::BACKSPACE => egui::Key::Backspace,
        global::Key::ENTER => egui::Key::Enter,
        global::Key::KP_ENTER => egui::Key::Enter,
        global::Key::INSERT => egui::Key::Insert,
        global::Key::DELETE => egui::Key::Delete,
        // global::Key::PAUSE => egui::Key::Pause,
        // global::Key::PRINT => egui::Key::Print,
        // global::Key::SYSREQ => egui::Key::Sysreq,
        // global::Key::CLEAR => egui::Key::Clear,
        global::Key::HOME => egui::Key::Home,
        global::Key::END => egui::Key::End,
        global::Key::LEFT => egui::Key::ArrowLeft,
        global::Key::UP => egui::Key::ArrowUp,
        global::Key::RIGHT => egui::Key::ArrowRight,
        global::Key::DOWN => egui::Key::ArrowDown,
        global::Key::PAGEUP => egui::Key::PageUp,
        global::Key::PAGEDOWN => egui::Key::PageDown,
        // global::Key::SHIFT => egui::Key::Shift,
        // global::Key::CTRL => egui::Key::Ctrl,
        // global::Key::META => egui::Key::Meta,
        // global::Key::ALT => egui::Key::Alt,
        // global::Key::CAPSLOCK => egui::Key::Capslock,
        // global::Key::NUMLOCK => egui::Key::Numlo,
        // global::Key::SCROLLLOCK => egui::Key::Scrol,
        global::Key::F1 => egui::Key::F1,
        global::Key::F2 => egui::Key::F2,
        global::Key::F3 => egui::Key::F3,
        global::Key::F4 => egui::Key::F4,
        global::Key::F5 => egui::Key::F5,
        global::Key::F6 => egui::Key::F6,
        global::Key::F7 => egui::Key::F7,
        global::Key::F8 => egui::Key::F8,
        global::Key::F9 => egui::Key::F9,
        global::Key::F10 => egui::Key::F10,
        global::Key::F11 => egui::Key::F11,
        global::Key::F12 => egui::Key::F12,
        global::Key::F13 => egui::Key::F13,
        global::Key::F14 => egui::Key::F14,
        global::Key::F15 => egui::Key::F15,
        global::Key::F16 => egui::Key::F16,
        global::Key::F17 => egui::Key::F17,
        global::Key::F18 => egui::Key::F18,
        global::Key::F19 => egui::Key::F19,
        global::Key::F20 => egui::Key::F20,
        // global::Key::F21 => egui::Key::F21,
        // global::Key::F22 => egui::Key::F22,
        // global::Key::F23 => egui::Key::F23,
        // global::Key::F24 => egui::Key::F24,
        // global::Key::F25 => egui::Key::F25,
        // global::Key::F26 => egui::Key::F26,
        // global::Key::F27 => egui::Key::F27,
        // global::Key::F28 => egui::Key::F28,
        // global::Key::F29 => egui::Key::F29,
        // global::Key::F30 => egui::Key::F30,
        // global::Key::F31 => egui::Key::F31,
        // global::Key::F32 => egui::Key::F32,
        // global::Key::F33 => egui::Key::F33,
        // global::Key::F34 => egui::Key::F34,
        // global::Key::F35 => egui::Key::F35,
        // global::Key::KP_MULTIPLY => egui::Key::KpMultiply,
        // global::Key::KP_DIVIDE => egui::Key::KpDivide,
        // global::Key::KP_SUBTRACT => egui::Key::KpSubtract,
        // global::Key::KP_PERIOD => egui::Key::KpPeriod,
        // global::Key::KP_ADD => egui::Key::KpAdd,
        global::Key::KP_0 => egui::Key::Num0,
        global::Key::KP_1 => egui::Key::Num1,
        global::Key::KP_2 => egui::Key::Num2,
        global::Key::KP_3 => egui::Key::Num3,
        global::Key::KP_4 => egui::Key::Num4,
        global::Key::KP_5 => egui::Key::Num5,
        global::Key::KP_6 => egui::Key::Num6,
        global::Key::KP_7 => egui::Key::Num7,
        global::Key::KP_8 => egui::Key::Num8,
        global::Key::KP_9 => egui::Key::Num9,
        // global::Key::MENU => egui::Key::Menu,
        // global::Key::HYPER => egui::Key::Hyper,
        // global::Key::HELP => egui::Key::Help,
        // global::Key::BACK => egui::Key::Back,
        // global::Key::FORWARD => egui::Key::Forward,
        // global::Key::STOP => egui::Key::Stop,
        // global::Key::REFRESH => egui::Key::Refresh,
        // global::Key::VOLUMEDOWN => egui::Key::Volumedown,
        // global::Key::VOLUMEMUTE => egui::Key::Volumemute,
        // global::Key::VOLUMEUP => egui::Key::Volumeup,
        // global::Key::MEDIAPLAY => egui::Key::Mediaplay,
        // global::Key::MEDIASTOP => egui::Key::Mediastop,
        // global::Key::MEDIAPREVIOUS => egui::Key::Mediaprevious,
        // global::Key::MEDIANEXT => egui::Key::Medianext,
        // global::Key::MEDIARECORD => egui::Key::Mediarecord,
        // global::Key::HOMEPAGE => egui::Key::Homepage,
        // global::Key::FAVORITES => egui::Key::Favorites,
        // global::Key::SEARCH => egui::Key::Search,
        // global::Key::STANDBY => egui::Key::Standby,
        // global::Key::OPENURL => egui::Key::Openurl,
        // global::Key::LAUNCHMAIL => egui::Key::Launchmail,
        // global::Key::LAUNCHMEDIA => egui::Key::Launchmedia,
        // global::Key::LAUNCH0 => egui::Key::Launch0,
        // global::Key::LAUNCH1 => egui::Key::Launch1,
        // global::Key::LAUNCH2 => egui::Key::Launch2,
        // global::Key::LAUNCH3 => egui::Key::Launch3,
        // global::Key::LAUNCH4 => egui::Key::Launch4,
        // global::Key::LAUNCH5 => egui::Key::Launch5,
        // global::Key::LAUNCH6 => egui::Key::Launch6,
        // global::Key::LAUNCH7 => egui::Key::Launch7,
        // global::Key::LAUNCH8 => egui::Key::Launch8,
        // global::Key::LAUNCH9 => egui::Key::Launch9,
        // global::Key::LAUNCHA => egui::Key::Launcha,
        // global::Key::LAUNCHB => egui::Key::Launchb,
        // global::Key::LAUNCHC => egui::Key::Launchc,
        // global::Key::LAUNCHD => egui::Key::Launchd,
        // global::Key::LAUNCHE => egui::Key::Launche,
        // global::Key::LAUNCHF => egui::Key::Launchf,
        // global::Key::GLOBE => egui::Key::Globe,
        // global::Key::KEYBOARD => egui::Key::Keyboard,
        // global::Key::JIS_EISU => egui::Key::JisEisu,
        // global::Key::JIS_KANA => egui::Key::JisKana,
        // global::Key::UNKNOWN => egui::Key::Unknown,
        global::Key::SPACE => egui::Key::Space,
        // global::Key::EXCLAM => egui::Key::Exclam,
        // global::Key::QUOTEDBL => egui::Key::Quotedbl,
        // global::Key::NUMBERSIGN => egui::Key::Numbersign,
        // global::Key::DOLLAR => egui::Key::Dollar,
        // global::Key::PERCENT => egui::Key::Percent,
        // global::Key::AMPERSAND => egui::Key::Ampersand,
        // global::Key::APOSTROPHE => egui::Key::Apostrophe,
        // global::Key::PARENLEFT => egui::Key::Parenleft,
        // global::Key::PARENRIGHT => egui::Key::Parenright,
        // global::Key::ASTERISK => egui::Key::Asterisk,
        global::Key::PLUS => egui::Key::Plus,
        global::Key::COMMA => egui::Key::Comma,
        global::Key::MINUS => egui::Key::Minus,
        global::Key::PERIOD => egui::Key::Period,
        global::Key::SLASH => egui::Key::Slash,
        global::Key::KEY_0 => egui::Key::Num0,
        global::Key::KEY_1 => egui::Key::Num1,
        global::Key::KEY_2 => egui::Key::Num2,
        global::Key::KEY_3 => egui::Key::Num3,
        global::Key::KEY_4 => egui::Key::Num4,
        global::Key::KEY_5 => egui::Key::Num5,
        global::Key::KEY_6 => egui::Key::Num6,
        global::Key::KEY_7 => egui::Key::Num7,
        global::Key::KEY_8 => egui::Key::Num8,
        global::Key::KEY_9 => egui::Key::Num9,
        global::Key::COLON => egui::Key::Colon,
        global::Key::SEMICOLON => egui::Key::Semicolon,
        // global::Key::LESS => egui::Key::Less,
        // global::Key::EQUAL => egui::Key::Equal,
        // global::Key::GREATER => egui::Key::Greater,
        // global::Key::QUESTION => egui::Key::Question,
        // global::Key::AT => egui::Key::At,
        global::Key::A => egui::Key::A,
        global::Key::B => egui::Key::B,
        global::Key::C => egui::Key::C,
        global::Key::D => egui::Key::D,
        global::Key::E => egui::Key::E,
        global::Key::F => egui::Key::F,
        global::Key::G => egui::Key::G,
        global::Key::H => egui::Key::H,
        global::Key::I => egui::Key::I,
        global::Key::J => egui::Key::J,
        global::Key::K => egui::Key::K,
        global::Key::L => egui::Key::L,
        global::Key::M => egui::Key::M,
        global::Key::N => egui::Key::N,
        global::Key::O => egui::Key::O,
        global::Key::P => egui::Key::P,
        global::Key::Q => egui::Key::Q,
        global::Key::R => egui::Key::R,
        global::Key::S => egui::Key::S,
        global::Key::T => egui::Key::T,
        global::Key::U => egui::Key::U,
        global::Key::V => egui::Key::V,
        global::Key::W => egui::Key::W,
        global::Key::X => egui::Key::X,
        global::Key::Y => egui::Key::Y,
        global::Key::Z => egui::Key::Z,
        global::Key::BRACKETLEFT => egui::Key::OpenBracket,
        global::Key::BACKSLASH => egui::Key::Backslash,
        global::Key::BRACKETRIGHT => egui::Key::CloseBracket,
        // global::Key::ASCIICIRCUM => egui::Key::Asciicircum,
        // global::Key::UNDERSCORE => egui::Key::Underscore,
        // global::Key::QUOTELEFT => egui::Key::Quoteleft,
        global::Key::BRACELEFT => egui::Key::OpenBracket,
        // global::Key::BAR => egui::Key::Bar,
        global::Key::BRACERIGHT => egui::Key::CloseBracket,
        // global::Key::ASCIITILDE => egui::Key::Asciitilde,
        // global::Key::YEN => egui::Key::Yen,
        // global::Key::SECTION => egui::Key::Section,
        _ => return None,
    }
    .into()
}
