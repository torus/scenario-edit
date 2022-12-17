function submit(arg) {
    console.log("submit", arg)
    const line_divs = Array.from(document.querySelectorAll('.line-form'))
    console.log(line_divs)
    const lines = line_divs.map(el => {
        let charinput = el.querySelector('input.character-input')
        let lineinput = el.querySelector('input.line-input')
        const option_elems = el.querySelectorAll('.option-input-group')
        const options = Array.from(option_elems).map(el => {
            const text = el.querySelector(".option-input").value.trim()
            if (!text)
                return false;
            console.log(text);
            const flags_req = el.querySelector(".option-flags-req-input")
                  .value.split(/ +/).filter(val => val)
            const jump = [el.querySelector(".option-jump-input").value.trim()]
                  .filter(val => val)
            return {text: text, "flags-required": flags_req, "jump-to": jump}
        }).filter(val => val)

        if (lineinput.value.length > 0)
            return {character: charinput.value, text: lineinput.value, options}
        else
            return null
    }).filter(elem => elem)

    const flags_req = document.querySelector('#flags-req-input')
          .value.split(/ +/).filter(val => val)
    const flags_exc = document.querySelector('#flags-exc-input')
          .value.split(/ +/).filter(val => val)
    const flags_set = document.querySelector('#flags-set-input')
          .value.split(/ +/).filter(val => val)

    const data = {
        type:  document.querySelector('#type-input').value,
        label: document.querySelector('#label-input').value.trim(),
        location: document.querySelector('#location-input').value.trim(),
        trigger: document.querySelector('#trigger-input').value.trim(),
        "flags-required": flags_req,
        "flags-exclusive": flags_exc,
        "flags-set": flags_set,
        lines
    }

    if (data.type == 'portal') {
        data['portal-destination']
            = document.querySelector('#portal-destination').value
        data.lines = []
    }

    var input_elem
    if (input_elem = document.querySelector('#ord-input')) {
        data["ord"] = input_elem.value
    }
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

    const a = document.createElement('a')
    a.href = form.action
    const path = a.pathname.split('/')
    if (!path[4]) {
        path[4] = data.label
        form.action = path.join('/')
    }

    return form.submit()
}

function on_line_plus_button_clicked(ev) {
    console.log("plus line")
    const orig = document.querySelector("#hidden-field > div")
    console.log(orig)
    const newelem = orig.cloneNode(true)
        setup_line_form(newelem)
    const container = document.querySelector("#line-fields")
    container.append(newelem)
    return false
}

function on_add_option_button_clicked(ev) {
    console.log(ev)
}

function setup_line_form(elem) {
    const btn = elem.querySelector(".add-option-button")
    btn.addEventListener("click", ev => {
        const container = elem.querySelector(".option-fields")
        const new_elem = document.querySelector("#hidden-option-field > div")
              .cloneNode(true)
        container.append(new_elem)
    })
}

function setup_form(form) {
    form.onsubmit = submit

    const plus_button = document.querySelector("#add-line-button")
    plus_button.addEventListener("click", on_line_plus_button_clicked)

    const line_forms = document.querySelectorAll(".line-form")
    line_forms.forEach(setup_line_form)

    const type_input = document.querySelector("#type-input")
    const dest_input = document.querySelector("#portal-destination-input")

    const update_form = () => {
        if (type_input.value == 'portal') {
            plus_button.classList.add('is-hidden')
            dest_input.classList.remove('is-hidden')
        } else {
            plus_button.classList.remove('is-hidden')
            dest_input.classList.add('is-hidden')
        }
    }

    type_input.addEventListener("change", update_form)
    update_form()
}

function update_title_by_hash(anchor) {
    const a = anchor || document.createElement('a')
    if (!anchor)
        a.setAttribute('href', location.href)
    if (!a.hash)
        return

    const elem = document.querySelector(decodeURI(a.hash))
    console.log('update_title_by_hash', elem)
    if (elem) {
        const title = elem.querySelector('h4').innerText.trim()
        document.title = title
    }
}

const form = document.querySelector('#edit-form')
if (form)
    setup_form(form)

if (/\/play\/\d+$/.test(location.href)) {
    const link = document.querySelector('a[trigger="#description"]')
    console.log(link)
    link.click()
} else {
    const link = document.querySelector('a[trigger="#description"]')
    if (link) {
        link.innerHTML =
            '<span class="icon"><i class="fas fa-lightbulb"></i></span>'
            + '場所の悦明'
    }
}

document.querySelectorAll('a.anchor').forEach(e => {
    e.addEventListener('click', ev => update_title_by_hash(e))
})

document.querySelectorAll('button.dialog-detail-button').forEach(e => {
  const [_, scenario_id, dialog_id] = e.id.match(/dialog-detail-(\d+)-(\d+)/)
  const proc = () => {
    const request = new XMLHttpRequest()
    request.open('GET', `/scenarios/${scenario_id}/dialog-details/${dialog_id}`)
    request.onload = () => {
      const elem = document.querySelector(`#dialog-detail-container-${dialog_id}`)
      elem.innerHTML = request.response
    }
    request.send()
  }
  e.addEventListener('click', proc)

  // automatic load
  const obs = new IntersectionObserver((entries) => {
    if (entries[0].intersectionRatio <= 0) return
    proc()
  })
  obs.observe(e)
})

update_title_by_hash()

console.log("hello")
