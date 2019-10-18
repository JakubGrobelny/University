fn square_area_to_circle(area: f64) -> f64 {
    let radius: f64 = area.sqrt() / 2.0;
    radius * radius * std::f64::consts::PI 
}

fn assert_close(a:f64, b:f64, epsilon:f64) {
    assert!( (a-b).abs() < epsilon, format!("Expected: {}, got: {}",b,a) );
}

#[test]
fn test0() {
    assert_close(square_area_to_circle(0.0), 0.0, 1e-8)
}

#[test]
fn test1() {
    assert_close(square_area_to_circle(4.0), 3.141592654, 1e-4)
}

#[test]
fn test2() {
    assert_close(square_area_to_circle(9.0), 7.068583470577, 1e-4)
}

#[test]
fn test3() {
    assert_close(square_area_to_circle(12.5), 9.817477042468, 1e-4)
}

#[test]
fn test4() {
    assert_close(square_area_to_circle(1.25), 0.98174770424, 1e-8)
}


