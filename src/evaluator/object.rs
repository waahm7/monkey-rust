use std::fmt;

type ObjectType = String;

pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
}
impl Object {
    pub fn inspect(&self) -> String {
        return match self {
            Object::Integer(i) => i.to_string(),
            Object::Boolean(i) => i.to_string(),
            Object::Null => String::from("NULL"),
        };
    }

    pub fn get_type(&self) -> String {
        match self {
            Object::Integer(_) => String::from("INTEGER"),
            Object::Boolean(_) => String::from("BOOLEAN"),
            Object::Null => String::from("NULL"),
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{}", self.inspect())
    }
}
