import { reactShinyInput } from 'reactR';
import { useState, useRef } from 'react';
import IonRangeSlider from 'react-ion-slider';
import shortid from 'shortid';
import ReactMarkdown from 'react-markdown';

function get_ratio(a, b) {
  var vals;
  if( a === 0 & b === 0) {
    vals = [0.5, 0.5];
  } else if (b === 0) {
    vals = [1, 0];
  } else {
    vals = [ a/b, 1 ].map((i) => i/(1 + a/b));
  }
  return vals;
}

function state_fix(a, b) {
  if (a === b & (a*2) % 2 & (b*2) % 2) {
    a = Math.floor(a);
    b = Math.ceil(b);
  } else {
    a = Math.round(a);
    b = Math.round(b);
  }
  return [a,b];
}

function calculate_state(state, last_moved) {
  var multipliers, available, proposed_values = [0,0];
  if(last_moved === 0) {
    multipliers = get_ratio(state[1], state[2]);
    available = 100 - state[0];
    for(var i = 0; i < 2; i++) {
      proposed_values[i] = multipliers[i]*available
    }
    proposed_values = state_fix(proposed_values[0], proposed_values[1]);
    state[1] = proposed_values[0]; state[2] = proposed_values[1];
  } else if (last_moved === 1) {
    available = 100 - state[0];
    let val1 = state[1], val2 = state[2], val3 = state[0];
    if (val1 > available) {
      let diff = val1 - available;
      val3 = val3 - diff
      val2 = 0;
    } else {
      val2 = 100 - val3 - val1;
    }

    state[1] = val1; state[2] = val2; state[0] = val3;
  } else {
    available = 100 - state[0];
    let val1 = state[1], val2 = state[2], val3 = state[0];
    if (val2 > available) {
      let diff = val2 - available;
      val3 = val3 - diff
      val1 = 0
    } else {
      val1 = 100 - val3 - val2;
    }

    state[1] = val1; state[2] = val2; state[0] = val3;
  }
}

const TripleSliderInput = ({ configuration, value, setValue }) => {

  let sliders = [0,1,2].map((i) => useRef(null));
  let id1 = shortid.generate();
  let id2 = shortid.generate();
  let id3 = shortid.generate();

  const ionStyle = {
    display: "inline-block",
    'vertical-align': "middle",
    'max-width': "25%",
    'min-width': "20%",
    'padding-right': "1.5rem"
  };

  const tinputStyle = {
    display: "inline-block",
    'vertical-align': "middle",
    'padding-top': "2%",
    'max-width': "10%"
  };

  const contentStyle = {
    "display": "inline-block",
    "vertical-align": "middle",
    "padding-top": "2%",
    "max-width": "62%"
  }

  const handle_ion = (s_value, ix) => {
    let state = value.state;
    state[ix] = s_value.from;
    calculate_state(state, ix);
    for(var i = 0; i < 3; i++) {
      if(i !== ix){
        sliders[i].current.update({from: state[i]});
      }
    }
    setValue({
      ...value,
      state: state
    });
  }

  const handle_numeric = (evt, ix) => {
    let val = evt.target.value;
    val = val > 100 ? 100 : val;
    val = val < 0 ? 0 : val;
    let state = value.state;
    state[ix] = val;
    calculate_state(state, ix);
    for(var i = 0; i < 3; i++) {
      sliders[i].current.update({from: state[i]});
    }
    setValue({
      ...value,
      state: state
    });
  }

  return  (
  <div>
    <div>
      <div className={'react-slider'}>
        <label htmlFor={id1}>{value.label[0]}</label>
        <IonRangeSlider id={id1} type={"single"} min={0} max={100} from={value.state[0]} step={1} onChange={(x) => handle_ion(x,0)} ref={sliders[0]} grid={true} postfix={"%"}/>
      </div>
      <div className={'react-tbox'}>
        <input type="number" value={value.state[0]} min={0} max={100} onChange={(evt) => handle_numeric(evt, 0)}/>
      </div>
      <div className={'react-content'}>
        <ReactMarkdown source={value.content[0]} />
      </div>
    </div>
    <div>
      <div className={'react-slider'}>
        <label htmlFor={id2}>{value.label[1]}</label>
        <IonRangeSlider id={id2} type={"single"} min={0} max={100} from={value.state[1]} step={1} onChange={(x) => handle_ion(x,1)} ref={sliders[1]}  grid={true} postfix={"%"} />
      </div>
      <div className={'react-tbox'}>
        <input type="number" value={value.state[1]} min={0} max={100} onChange={(evt) => handle_numeric(evt, 1)}/>
      </div>
      <div className={'react-content'}>
        <ReactMarkdown source={value.content[1]} />
      </div>
    </div>
    <div>
      <div className={'react-slider'}>
        <label htmlFor={id3}>{value.label[2]}</label>
        <IonRangeSlider id={id3} type={"single"} min={0} max={100} from={value.state[2]} step={1} onChange={(x) => handle_ion(x,2)} ref={sliders[2]}  grid={true} postfix={"%"} />
      </div>
      <div className={'react-tbox'}>
        <input type="number" value={value.state[2]} min={0} max={100} onChange={(evt) => handle_numeric(evt, 2)}/>
      </div>
      <div className={'react-content'}>
        <ReactMarkdown source={value.content[2]} />
      </div>
    </div>
  </div>


  )
};

reactShinyInput('.customSliderTriple', 'customSlider.customSliderTriple', TripleSliderInput);
