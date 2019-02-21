function change_state_if_invalid(element, re, content) {
    if (re.test(content)) {
        element.classList.remove('invalid');
        return true;
    } 
    else {
        element.classList.add('invalid');
        return false;
    }
}

function does_pesel_match_birthdate(date, pesel) {
    if (!date)
        return false;

    d_day = date.getUTCDate();
    d_month = date.getMonth() + 1;
    d_year = date.getYear();

    p_day = pesel.substring(4, 6);
    p_month = pesel.substring(2, 4);
    p_year = pesel.substring(0, 2);

    return (d_day == p_day) && (d_month == p_month) && (d_year == p_year);
}

function validate_account_number() {
    // CC AAAAAAAA BBBBBBBBBBBBBBBB - 26 cyfr
    var acc_id = document.getElementById('acc_id');
    var number = acc_id.value.replace(/\s/g, '');
    return change_state_if_invalid(acc_id, /^[0-9]{26}$/, number);
}

function validate_pesel() {
    // BBBBBBSSSSC - 11 cyfr
    var pesel = document.getElementById('pesel');
    var value = pesel.value;

    if (!change_state_if_invalid(pesel, /^[0-9]{11}$/, value)) {
        return false;
    }

    var digits = value.substring(0, 10).split('');
    digits = digits.map((x) => Number(x));
    var multipliers = [9, 7, 3, 1, 9, 7, 3, 1, 9, 7];

    for (let i = 0; i < multipliers.length; i++) {
        digits[i] *= multipliers[i];
    }
    
    var correct = digits.reduce((prev, current, i, a) => (prev + current)) % 10;
    
    var birthdate = document.getElementById('birthdate').valueAsDate;

    if (correct != value[10] || !does_pesel_match_birthdate(birthdate, value)) {
        pesel.classList.add('invalid');
        return false;
    }

    return true;
}

function validate_birthdate() {
    var birthdate = document.getElementById('birthdate');
    var now = new Date();
    
    if (birthdate.valueAsDate > now) {
        birthdate.classList.add('invalid');
        return false;
    }
    else {
        birthdate.classList.remove('invalid');
        validate_pesel();
        return true;
    }
}

function validate_email() {
    const email_regex = /^(([^<>()\[\]\\.,;:\s@"]+(\.[^<>()\[\]\\.,;:\s@"]+)*)|(".+"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/;
    var email = document.getElementById('email');
    return change_state_if_invalid(email, email_regex, email.value);
}

function validate_form() {
    if (validate_account_number() &&
        validate_pesel() &&
        validate_birthdate() &&
        validate_email()
    ) {
        // do something
    }
}
