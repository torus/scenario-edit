console.log("hello")

function submit(arg) {
    console.log("submit", arg)
    const line_divs = Array.from(document.querySelectorAll('.line-form'))
    console.log(line_divs)
    const lines = line_divs.map(el => {
        let charinput = el.querySelector('input.character-input')
        let lineinput = el.querySelector('input.line-input')
        if (charinput.value.length > 0 && lineinput.value.length > 0)
            return {character: charinput.value, text: lineinput.value}
        else
            return null
    }).filter(elem => elem)

    const data = {
        type:  document.querySelector('#type-input').value,
        label: document.querySelector('#label-input').value,
        section: document.querySelector('#location-input').value,
        lines
    }

    var input_elem
    if (input_elem = document.querySelector('#original-label-input')) {
        data["original-label"] = input_elem.value
    }
    if (input_elem = document.querySelector('#prev-label-input')) {
        data["previous-label"] = input_elem.value
    }

    console.log(data)

    const form = document.querySelector('#edit-form')
    const elem = document.createElement('input')
    elem.setAttribute('type', 'hidden')
    elem.setAttribute('name', 'json')
    elem.value = JSON.stringify(data)
    form.append(elem)

    return form.submit()
}

const form = document.querySelector('#edit-form')
if (form)
    form.onsubmit = submit

function on_line_plus_button_clicked(ev) {
    console.log("plus line")
    const orig = document.querySelector("#hidden-field > div")
    console.log(orig)
    const newelem = orig.cloneNode(true)
    const container = document.querySelector("#line-fields")
    container.append(newelem)
    return false
}

const plus_button = document.querySelector("#add-line-button")
if (plus_button)
    plus_button.addEventListener("click", on_line_plus_button_clicked)
