#[repr(C)]
pub struct Thing {
    pub id: i64,
}

pub fn new_thing(id: i64) -> Thing {
    Thing { id }
}

pub fn another_new_thing(id: i64) -> Thing {
    Thing { id }
}
