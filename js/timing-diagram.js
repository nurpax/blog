var snabbdom = require('snabbdom');
var patch = snabbdom.init([ // Init patch function with chosen modules
  require('snabbdom/modules/attributes').default, // makes it easy to toggle classes
  require('snabbdom/modules/style').default,
]);
var h = require('snabbdom/h').default; // helper function for creating vnodes

const WIDTH = 384
const HEIGHT = 272

const ANIM_START_LINE = 48

// Covert a pixel position in the VICE screenshot resolution to actual C64 Y position
function pixYtoC64(y) {
  return y-35 + 51
}

function c64YtoPix(y) {
  return y-51 + 35
}

function translate(x, y) {
  return `translate(${x}, ${y})`
}

function makeCycleBlocks ({activeCycle, ...props}) {
  let res = []
  const BW = 5
  const BH = 5
  for (var i = 0; i < 63; i++) {
    const x = BW * i
    const y = 0
    const fill = activeCycle == i ? '#8f8' : '#444'
    const attrs = {
      fill,
      width: BW,
      height: BH,
      x, y,
      "stroke-width": 1,
      stroke: '#aaa'
    }
    const block = h('rect', {attrs})
    res.push(block)
  }
  return h('g', { attrs: {transform:translate(props.x, props.y)}}, res)
}

function rasterBeam({line, activeCycle}) {
  // raster hits visible screen area at cycle 12
  const x = ((activeCycle - 12) * 8) + 32
  return h('g', [
    h('rect', {attrs: {
      fill: '#fff',
      width: 2,
      height: 2,
      x,
      y: c64YtoPix(line)
    }})
  ])
  return
}

function bottomUI ({activeCycle, line}) {
  const col2style = {
    width: '20px',
    'margin-left': '1em'
  }
  const col3style = {
    'margin-left': '1em',
    'align-self': 'center'
  }
  let badline = ''
  if (line > 0x30 && (line & 7) == 3) {
    badline = h('span', {style: {fontSize:'1.3em', color:'#f00'}}, 'BAD LINE')
  }
  return h('div', {style: {display: 'flex'}}, [
    h('div', ['Clock cycle', h('br'),
              'Line', h('br')]),
    h('div', {style: col2style}, [
      `${activeCycle}`,
      h('br'),
      `${line} `
    ]),
    h('div', {style: col3style}, [badline])
  ])
}

class TimingDiagram {

  constructor () {
    this.vnode = null
    this.state = {
      line: ANIM_START_LINE,
      activeCycle: 0
    }
  }

  view (props) {
    var view = h('div', [
      h('svg', {attrs: {width: '100%', viewBox: `0 0 ${WIDTH} ${HEIGHT}`}}, [
        h('image', {attrs: {width:384, height: 272, href:'/images/bintris/c64-basic.png'}}),
        makeCycleBlocks({
          x: 20,
          y: 240,
          activeCycle:props.activeCycle
        }),
        rasterBeam(props)
      ]),
      bottomUI(props)
    ])
    return view
  }

  render (props) {
    this.vnode = patch(this.vnode, this.view(props))
  }

  mount (container) {
    this.vnode = patch(container, this.view(this.state))

    setInterval(cb => {
      this.render(this.state)

      this.state.activeCycle++
      if (this.state.activeCycle >= 63) {
        this.state.activeCycle = 0
        this.state.line++

        if (this.state.line >= ANIM_START_LINE+16) {
          this.state.line = ANIM_START_LINE
        }
      }
    }, 100)
  }
}

module.exports = { TimingDiagram };
