use std::sync::Arc;

use loro_internal::{HandlerTrait, LoroDoc, TextHandler};
use pyo3::{prelude::*, types::PyBytes};

#[pyclass]
struct Loro(LoroDoc);

#[pyclass]
struct LoroText(TextHandler);

#[pymethods]
impl Loro {
    #[new]
    pub fn __new__() -> Self {
        Self(LoroDoc::default())
    }

    pub fn get_text(&mut self, id: &str) -> LoroText {
        let text = self.0.get_text(id);
        LoroText(text)
    }

    pub fn import_snapshot(&mut self, snapshot: &[u8]) -> PyResult<()> {
        self.0.import(snapshot).unwrap();
        Ok(())
    }

    pub fn import_update_batch(&mut self, data: Vec<Vec<u8>>) -> PyResult<()> {
        if data.is_empty() {
            return Ok(());
        }
        self.0.import_batch(&data).unwrap();
        Ok(())
    }

    pub fn export_snapshot(&self, py: Python) -> PyResult<PyObject> {
        let snapshot = self.0.export_snapshot();
        Ok(PyBytes::new(py, &snapshot).into())
    }
}

#[pymethods]
impl LoroText {
    pub fn insert(&mut self, ctx: &Loro, pos: usize, value: &str) -> PyResult<()> {
        self.0
            .insert_with_txn(&mut ctx.0.txn().unwrap(), pos, value)
            .unwrap();
        Ok(())
    }

    pub fn value(&self) -> String {
        Arc::try_unwrap(self.0.get_value().into_string().unwrap()).unwrap_or_else(|x| (*x).clone())
    }
}

#[pymodule]
fn pyloro(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_class::<Loro>()?;
    m.add_class::<LoroText>()?;
    Ok(())
}
