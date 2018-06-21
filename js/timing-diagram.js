var snabbdom = require('snabbdom');
var patch = snabbdom.init([ // Init patch function with chosen modules
  require('snabbdom/modules/attributes').default, // makes it easy to toggle classes
  require('snabbdom/modules/style').default,
  require('snabbdom/modules/eventlisteners').default
]);
var h = require('snabbdom/h').default; // helper function for creating vnodes
var asm = require('./asm.js')
var util = require('./util.js')

class VdomDiagram {
  constructor (divId) {
    this.vnode = null
    this.selector = `#${divId}`
  }

  render (props) {
    this.vnode = patch(this.vnode, this.view(props))
  }
}

const MS_PER_CYCLE = 100

const YSCROLL = 3
const WIDTH = 384
const HEIGHT = 272

const ANIM_START_LINE = 59

// starts on raster line 59 (badline)
const raster_badline_str = `
.C:11ea  8D 20 D0    STA $D020
.C:11ed  8D 21 D0    STA $D021
.C:11f0  EA          NOP
.C:11f1  EA          NOP
.C:11f2  EA          NOP
.C:11f3  A2 06       LDX #$06
.C:11f5  BD 00 2A    LDA .colors,X
`

const raster_normal_str = `
.C:11f8  8D 20 D0    STA $D020
.C:11fb  8D 21 D0    STA $D021
.C:11fe  EA          NOP
.C:11ff  EA          NOP
.C:1200  EA          NOP
.C:1201  EA          NOP
.C:1202  EA          NOP
.C:1203  EA          NOP
.C:1204  EA          NOP
.C:1205  EA          NOP
.C:1206  EA          NOP
.C:1207  EA          NOP
.C:1208  EA          NOP
.C:1209  EA          NOP
.C:120a  EA          NOP
.C:120b  EA          NOP
.C:120c  EA          NOP
.C:120d  EA          NOP
.C:120e  EA          NOP
.C:120f  EA          NOP
.C:1210  EA          NOP
.C:1211  EA          NOP
.C:1212  EA          NOP
.C:1213  EA          NOP
.C:1214  EA          NOP
.C:1215  24 FE       BIT $FE
.C:1217  A2 07       LDX #$07
.C:1219  BD 00 2A    LDA .colors,X
`

let instructions = asm.parse(raster_badline_str + raster_normal_str)

/*
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
*/


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
  return badline ? (cycle <= 10) || (cycle >= 54) : true
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
  res.push(h('rect', {attrs:{
    x:activeCycle*BW+1+0.5,
    y:BH+1,
    width:1,
    height:4,
    fill: '#000'
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

function rasterBeam({line, activeCycle, fast}) {
  // raster hits visible screen area at cycle 12
  const x = cycleToXPos(activeCycle)
  // Reset CSS anim on scanline start
  const animClass = (activeCycle == 0 || !fast) ? "" : ".move-beam"
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

function bottomUI ({activeCycle, line, ...props}) {
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
    'justify-content':'center',
    display:'flex',
    'background-color':'rgb(177,158,255)'
  }
  return h('div', {style: {
    'flex-direction':'row',
    'flex-wrap': 'wrap',
    ...contStyle
    }}, [
    h('div', {style:{
      'flex-direction':'row', 'font-size':'0.8em',
      ...contStyle,
      'padding-bottom':'10px'
    }}, [
      h('div', ['Clock cycle', h('br'),
                'Line', h('br')]),
      h('div', {style: col2style}, [
        `${activeCycle}`,
        h('br'),
        `${line} `
      ]),
      h('div', {style: col3style}, [badline])
    ]),
    // controls
    h('div', {style: {
      ...contStyle,
      'padding-bottom':'10px'
    }}, [
      h('button', {on: {click:props.onPauseResume}}, !props.paused ? 'pause' : 'resume'),
      h('button', {on: {click:props.onFastSlow}}, !props.fast ? 'faster' : 'slower'),
      h('button', {on: {click:props.onStep1}}, 'step 1')
    ])
  ])
}

// "monitor" window
function makeAssembly ({activeCycle, badline, insnIndex, ...props}) {
  const WIND_W = 140
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
    }}, insn.address+': ')
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
  constructor (selector) {
    super(selector)
    this.state = {
      line: ANIM_START_LINE,
      activeCycle: 0,
      insnCycle: 0,
      insnIndex: 0,
      paused: false,
      fast: true,
      tickCount: 0
    }

    this.onPauseResumeClick = this.onPauseResumeClick.bind(this)
    this.onFastSlowClick = this.onFastSlowClick.bind(this)
    this.onStep1Click = this.onStep1Click.bind(this)
  }

  cycleTicks () {
    return this.state.fast ? 1 : 10
  }

  onPauseResumeClick() {
    this.state.paused = !this.state.paused
    this.render(this.state)
  }

  onFastSlowClick() {
    this.state.fast = !this.state.fast
    this.state.tickCount = this.cycleTicks()
    this.state.paused = false
    this.render(this.state)
  }

  onStep1Click() {
    this.state.paused = true
    this.nextCycle()
    this.render(this.state)
  }

  view (props) {
    const badline = isBadLine(props.line)
    var view = h(`div${this.selector}`, [
      h('svg', {style:{display:'block'}, attrs: {width: '100%', viewBox: `0 0 ${WIDTH} ${HEIGHT}`}}, [
        h('image', {attrs: {class:'img-pixelated', width:384, height: 272, 'xlink:href':'/images/bintris/c64-basic.png'}}),
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
      bottomUI({
        onPauseResume:this.onPauseResumeClick,
        onFastSlow:this.onFastSlowClick,
        onStep1:this.onStep1Click,
        ...props
      })
    ])
    return view
  }

  nextLine () {
    this.state.activeCycle = 0
    this.state.line++

    if (this.state.line >= ANIM_START_LINE+2) {
      this.state.line = ANIM_START_LINE
      this.state.insnCycle = 0
      this.state.insnIndex = 0
      return true
    }
    return false
  }

  nextCycle () {
    let wrap = false
    this.state.activeCycle++
    if (this.state.activeCycle >= 63) {
      wrap = this.nextLine()
    }

    // Execute instructions if not "stunned"
    if (!wrap && isRunning(isBadLine(this.state.line), this.state.activeCycle)) {
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
  }


  mount () {
    const container = document.querySelector(`div${this.selector}`)
    this.vnode = patch(container, this.view(this.state))

    // TODO don't loop the anim.. burns battery on mobile
    setInterval(cb => {
      const isInViewport = util.isScrolledIntoView(this.vnode.elm)
      if (isInViewport && !this.state.paused) {
        this.state.tickCount--;
        if (this.state.tickCount <= 0) {
          this.nextCycle()
          this.render(this.state)
          this.state.tickCount = this.cycleTicks()
        }
      }
    }, MS_PER_CYCLE)
  }
}

class FldDiagram extends VdomDiagram{
  constructor (selector) {
    super(selector)
    this.state = {
    }
  }

  view (props) {
    const mkimg = (cls) => {
      return h('image', {attrs: {class:`img-pixelated ${cls}`, width:384, height: 272, 'xlink:href':'/images/bintris/c64-basic.png'}})
    }
    const imgTop = mkimg('c64-fld-top')
    const imgBottom = mkimg('c64-fld-bottom')
    const imgBottomBorder = mkimg('c64-fld-bottom-border')
    var view = h(`div${this.selector}`, [
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

  mount () {
    const container = document.querySelector(`div${this.selector}`)
    this.vnode = patch(container, this.view(this.state))
  }
}

class LogoWarpCrop extends VdomDiagram{
  constructor (selector) {
    super(selector)
    this.state = {
    }
  }

  view (props) {
    const mkimg = (cls) => {
      return h('image', {attrs: {class:`img-pixelated ${cls}`, width:384, height: 272, 'xlink:href':'/images/bintris/bintris-logo-wobble.gif'}})
    }
    var view = h(`div${this.selector}`, [
      h('svg', {style:{display:'block', backgroundColor:'#000'}, attrs: {
        width: '100%', viewBox: `40 0 ${WIDTH/2.5} ${HEIGHT/2.5}`
      }},
      [
        mkimg('logo-warp-crop')
      ])
    ])
    return view
  }

  mount () {
    const container = document.querySelector(`div${this.selector}`)
    this.vnode = patch(container, this.view(this.state))
  }
}

module.exports = { TimingDiagram, FldDiagram, LogoWarpCrop };
