use string_interner::{DefaultSymbol, StringInterner, backend::StringBackend};

pub type Interner = StringInterner<InternBackend>;
pub type InternBackend = StringBackend<InternSymbol>;
pub type InternSymbol = DefaultSymbol;
