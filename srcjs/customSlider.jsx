import { reactShinyInput } from 'reactR';
import { useState, useRef } from 'react';
import IonRangeSlider from 'react-ion-slider';
import shortid from 'shortid';
import ReactMarkdown from 'react-markdown';

const SingleSliderInput = ({ configuration, value, setValue }) => {

  const sliderEl = useRef(null);
  let id = shortid.generate();

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

  const handle_ion = (val) => {
    setValue({
      ...value,
      state: val.from
    });
  }

  const handle_numeric = (evt) => {
    let val = evt.target.value;
    val = val > 100 ? 100 : val;
    val = val < 0 ? 0 : val;
    sliderEl.current.update({from : val})
    setValue({
      ...value,
      state: val
    });
  }

  return  (
  <div>
    <div className={'react-slider'}>
      <label htmlFor={id}>{value.label}</label>
      <IonRangeSlider id={id} type={"single"} min={0} max={100} from={value.state} step={1} onChange={handle_ion} ref={sliderEl}  grid={true} postfix={"%"} />
    </div>
    <div className={'react-tbox'}>
      <input type="number" value={value.state} min={0} max={100} onChange={handle_numeric}/>
    </div>
    <div className={'react-content'}>
      <ReactMarkdown source={value.content} />
    </div>
  </div>
  )
};

reactShinyInput('.customSlider', 'customSlider.customSlider', SingleSliderInput);
