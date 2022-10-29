use parser_nom::tokenizer::tokenize;
use parser_nom::shunting_yard::to_rpn;
use wasm_bindgen::prelude::*;
#[wasm_bindgen]
extern "C" {
    pub fn alert(s: &str);
    // Use `js_namespace` here to bind `console.log(..)` instead of just
    // `log(..)`
    #[wasm_bindgen(js_namespace = console)]
    pub fn log(s: &str);
}

#[wasm_bindgen]
pub fn greet(name: &str) {
    alert(&format!("Hello, {}!", name));
}

#[wasm_bindgen]
pub fn ff_parse(name: &str) {
    match tokenize(name) {
        Ok(result) => {

            log(&format!("SUCCESS, {:?}", result));
            log(&format!("{:?}",to_rpn(&result)));
        }
        Err(e) => {
            log(&format!("FAIL, {}", e));
        }
    }
}
#[wasm_bindgen]
pub fn fff_parse(name: &str) ->JsValue{
    match tokenize(name) {
        Ok(result) => {


            JsValue::from_str(    &format!("{:?}",to_rpn(&result)))
        }
        Err(e) => {
            JsValue::from_str(   &format!("FAIL, {}", e))
        }
    }
}
#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
