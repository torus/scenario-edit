console.log("hello")

function submit(arg) {
    console.log("submit", arg)
    const line_divs = Array.from(document.querySelectorAll('.line-form'))
    console.log(line_divs)
    const lines = line_divs.map(el => {
        let charinput = el.querySelector('input.character-input')
        let lineinput = el.querySelector('input.line-input')
        return {character: charinput.value, text: lineinput.value}
    })

    const data = {
        type:  document.querySelector('#type-input').value,
        label: document.querySelector('#label-input').value,
        section: document.querySelector('#location-input').value,
        lines
    }

    if (document.querySelector('#original-label-input')) {
        data["original-label"] = document.querySelector('#original-label-input').value
    }
    if (document.querySelector('#prev-label-input')) {
        data["previous-label"] = document.querySelector('#original-label-input').value
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
