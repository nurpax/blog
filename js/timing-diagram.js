var snabbdom = require('snabbdom');
var patch = snabbdom.init([ // Init patch function with chosen modules
  require('snabbdom/modules/attributes').default, // makes it easy to toggle classes
  require('snabbdom/modules/style').default,
]);
var h = require('snabbdom/h').default; // helper function for creating vnodes
var asm = require('./asm.js')

class VdomDiagram {
  constructor () {
    this.vnode = null
  }

  render (props) {
    this.vnode = patch(this.vnode, this.view(props))
  }
}

const MS_PER_CYCLE = 100

const YSCROLL = 3
const WIDTH = 384
const HEIGHT = 272

const ANIM_START_LINE = -3+51 + 3*8

const instructions_str = `
.C:0900   .irq0:
.C:0900  8D 79 09    STA $0979
.C:0903  8E 7B 09    STX $097B
.C:0906  8C 7D 09    STY $097D
.C:0909  A9 22       LDA #$22
.C:090b  A2 09       LDX #$09
.C:090d  8D FE FF    STA $FFFE
.C:0910  8E FF FF    STX $FFFF
.C:0913  EE 12 D0    INC $D012
.C:0916  0E 19 D0    ASL $D019
.C:0919  BA          TSX
.C:091a  58          CLI
.C:091b  EA          NOP
.C:091c  EA          NOP
.C:091d  EA          NOP
.C:091e  EA          NOP
.C:091f  EA          NOP
.C:0920  EA          NOP
.C:0921  EA          NOP
.C:0922   .irq1:
.C:0922  9A          TXS
.C:0923  A2 08       LDX #$08
.C:0925  CA          DEX
.C:0926  D0 FD       BNE $0925
.C:0928  24 00       BIT $00
.C:092a  AD 12 D0    LDA $D012
.C:092d  CD 12 D0    CMP $D012
.C:0930  F0 00       BEQ $0932
.C:0932  EA          NOP
.C:0933  EA          NOP
.C:0934  EA          NOP
.C:0935  EA          NOP
.C:0936  EE 21 D0    INC $D021
.C:0939  EE 21 D0    INC $D021
.C:093c  EE 21 D0    INC $D021
.C:093f  EE 21 D0    INC $D021
.C:0942  EE 21 D0    INC $D021
.C:0945  EE 21 D0    INC $D021
.C:0948  A9 00       LDA #$00
.C:094a  8D 21 D0    STA $D021
.C:094d  24 FE       BIT $FE
.C:094f  EA          NOP
.C:0950  EA          NOP
.C:0951  EA          NOP
`

let instructions = asm.parse(instructions_str)

// Covert a pixel position in the VICE screenshot resolution to actual C64 Y position
function pixYtoC64(y) {
  return y-35 + 51
}

function c64YtoPix(y) {
  return y-51 + 35
}

function isBadLine(line) {
  return (line >= 0x30 && line <= 0xf7) && (line & 7) == YSCROLL
}

function translate(x, y) {
  return `translate(${x}, ${y})`
}

function isRunning(badline, cycle) {
  return badline ? (cycle < 12) || (cycle >= 54) : true
}

function makeCycleBlocks ({activeCycle, badline, ...props}) {
  let res = []
  const BW = 5
  const BH = 5
  for (var i = 0; i < 63; i++) {
    const x = BW * i
    const y = 0
    const executing = isRunning(badline, i)
    let fill = activeCycle == i ? '#fff' : '#2e2'
    if (!executing) {
      fill = '#000'
    }
    const attrs = {
      fill,
      width: BW-1,
      height: BH-1,
      x, y
    }
    const block = h('rect', {attrs})
    res.push(block)
  }
  res.push(h('text.status', {attrs:{
    x:activeCycle*BW,
    y:BH+10
  }}, isRunning(badline, activeCycle) ? `cycle ${activeCycle}` : 'stunned'))
  return h('g', { attrs: {transform:translate(props.x, props.y)}}, res)
}

function cycleToXPos(activeCycle) {
  return ((activeCycle - 12) * 8) + 32 - 0x18
}

function makeFetchBlocks ({activeCycle, badline, line}) {
  const y = c64YtoPix(((line-YSCROLL) & ~7) + YSCROLL)
  const rects = [
    h('rect', {attrs:{
      x:0,
      y,
      width:WIDTH,
      height:8,
      fill: badline ? '#f00' : '#0f0',
      opacity:0.2
    }})
  ]
  return h('g', rects)
}

function rasterBeam({line, activeCycle}) {
  // raster hits visible screen area at cycle 12
  const x = cycleToXPos(activeCycle)
  // Reset CSS anim on scanline start
  const animClass = activeCycle == 0 ? "" : ".move-beam"
  return h('g', [
    h(`rect${animClass}`, {attrs: {
      fill: '#fff',
      width: 2,
      height: 2,
      transform:translate(x-0.5, -0.5),
      x:0,
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
    'margin-left': '1.5em',
    'align-self': 'center',
    'min-width':'8.0em',
    fontSize:'1.1em'
  }
  let badline = ''
  if (isBadLine(line)) {
    badline = h('span', {style: {fontSize:'1.0em', color:'#f00'}}, 'BAD LINE')
  }
  const contStyle = {
    'font-family': 'C64 Pro Local',
    'letter-spacing':'1px',
    display: 'flex',
    'padding-bottom':'1em',
    'justify-content':'center',
    'background-color':'rgb(177,158,255)',
    fontSize:'0.8em'
  }
  return h('div', {style: contStyle}, [
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

// "monitor" window
function makeAssembly ({activeCycle, badline, insnIndex, ...props}) {
  const WIND_W = 130
  const WIND_H = 140
  const wind = h('rect', {attrs:{
    x:0, y:0,
    width:WIND_W,
    height:WIND_H,
    fill: '#000',
    opacity: 0.7,
    'stroke-width':'1px',
    stroke: '#fff'
  }})
  const ni = 8
  const min = insnIndex - ni < 0 ? 0 : insnIndex - ni
  let max = min + 2*ni
  if (max >= instructions.length) {
    max = instructions.length
  }
  const insns = instructions.slice(min, max)
  const ins = insns.map((insn,idx) => {
    const y = 10 + idx*8
    const fill = insnIndex == min+idx ? '#fff' : '#aaa'
    const addr = h('text.asm', {attrs: {
      x:3,
      y,
      fill
    }}, insn.address+':')
    const asm = h('text.asm', {attrs: {
      x:80,
      y,
      fill
    }}, insn.asm)
    const enc = h('text.asm', {attrs: {
      x:28,
      y,
      fill
    }}, insn.encoded)
    return h('g', [addr, asm, enc])
  })
  return h('g', { attrs: {transform:translate(props.x, props.y)}}, [wind, ...ins])
}

class TimingDiagram extends VdomDiagram{
  constructor () {
    super()
    this.state = {
      line: ANIM_START_LINE,
      activeCycle: 0,
      insnCycle: 0,
      insnIndex: 0
    }
  }

  view (props) {
    const badline = isBadLine(props.line)
    var view = h('div', [
      h('svg', {style:{display:'block'}, attrs: {width: '100%', viewBox: `0 0 ${WIDTH} ${HEIGHT}`}}, [
        h('image', {attrs: {class:'img-pixelated', width:384, height: 272, href:'/images/bintris/c64-basic.png'}}),
        makeCycleBlocks({
          x: 34,
          y: 240,
          badline,
          activeCycle: props.activeCycle
        }),
        makeAssembly({
          x:240,
          y:80,
          badline,
          insnIndex: props.insnIndex,
          activeCycle: props.activeCycle
        }),
        makeFetchBlocks({...props, badline}),
        rasterBeam(props)
      ]),
      bottomUI(props)
    ])
    return view
  }

  nextLine () {
    this.state.insnCycle = 0
    this.state.insnIndex = 0
    this.state.activeCycle = 0
    this.state.line++

    if (this.state.line >= ANIM_START_LINE+16) {
      this.state.line = ANIM_START_LINE
    }
  }

  nextCycle () {
    this.state.activeCycle++

    // Execute instructions if not "stunned"
    if (isRunning(isBadLine(this.state.line), this.state.activeCycle)) {
      const curInsn = instructions[this.state.insnIndex]
      this.state.insnCycle++
      if (this.state.insnCycle >= curInsn.timing.length) {
        this.state.insnCycle = 0
        this.state.insnIndex++
        if (this.state.insnIndex >= instructions.length) {
          console.error('insnIndex wrap around, shouldn\'t happen')
        }
      }
    }
    if (this.state.activeCycle >= 63) {
      this.nextLine()
    }
  }

  mount (container) {
    this.vnode = patch(container, this.view(this.state))

    // TODO don't loop the anim.. burns battery on mobile
    setInterval(cb => {
      this.render(this.state)

      this.nextCycle()
    }, MS_PER_CYCLE)
  }
}

class FldDiagram extends VdomDiagram{
  constructor () {
    super()
    this.state = {
    }
  }

  view (props) {
    const mkimg = (cls) => {
      return h('image', {attrs: {class:`img-pixelated ${cls}`, width:384, height: 272, href:'/images/bintris/c64-basic.png'}})
    }
    const imgTop = mkimg('c64-fld-top')
    const imgBottom = mkimg('c64-fld-bottom')
    const imgBottomBorder = mkimg('c64-fld-bottom-border')
    var view = h('div', [
      h('svg', {style:{display:'block', backgroundColor:'#000'}, attrs: {
        width: '100%', viewBox: `0 0 ${WIDTH} ${HEIGHT}`
      }},
      [
        imgTop,
        imgBottom,
        imgBottomBorder,
        h('rect', {attrs:{x:0,y:0,width:32, height:HEIGHT, fill:'rgb(177,158,255)'}}),
        h('rect', {attrs:{x:WIDTH-32,y:0,width:32, height:HEIGHT, fill:'rgb(177,158,255)'}})
      ])
    ])
    return view
  }

  mount (container) {
    this.vnode = patch(container, this.view(this.state))
  }
}

class LogoWarpCrop extends VdomDiagram{
  constructor () {
    super()
    this.state = {
    }
  }

  view (props) {
    const mkimg = (cls) => {
      return h('image', {attrs: {class:`img-pixelated ${cls}`, width:384, height: 272, href:'/images/bintris/bintris-logo-wobble.gif'}})
    }
    var view = h('div', [
      h('svg', {style:{display:'block', backgroundColor:'#000'}, attrs: {
        width: '100%', viewBox: `40 0 ${WIDTH/2.5} ${HEIGHT/2.5}`
      }},
      [
        mkimg('logo-warp-crop')
      ])
    ])
    return view
  }

  mount (container) {
    this.vnode = patch(container, this.view(this.state))
  }
}

module.exports = { TimingDiagram, FldDiagram, LogoWarpCrop };
