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
            Object::Null => String::from("null"),
        };
    }

    pub fn get_type(&self) -> String {
        match self {
            Object::Integer(_) => String::from("Integer"),
            Object::Boolean(_) => String::from("Boolean"),
            Object::Null => String::from("null"),
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{}", self.inspect())
    }
}
