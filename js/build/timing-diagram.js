(function(f){if(typeof exports==="object"&&typeof module!=="undefined"){module.exports=f()}else if(typeof define==="function"&&define.amd){define([],f)}else{var g;if(typeof window!=="undefined"){g=window}else if(typeof global!=="undefined"){g=global}else if(typeof self!=="undefined"){g=self}else{g=this}g.diagrams = f()}})(function(){var define,module,exports;return (function(){function r(e,n,t){function o(i,f){if(!n[i]){if(!e[i]){var c="function"==typeof require&&require;if(!f&&c)return c(i,!0);if(u)return u(i,!0);var a=new Error("Cannot find module '"+i+"'");throw a.code="MODULE_NOT_FOUND",a}var p=n[i]={exports:{}};e[i][0].call(p.exports,function(r){var n=e[i][1][r];return o(n||r)},p,p.exports,r,e,n,t)}return n[i].exports}for(var u="function"==typeof require&&require,i=0;i<t.length;i++)o(t[i]);return o}return r})()({1:[function(require,module,exports){

const R  = 'R'
const W  = 'W'
const RW = 'RW'

function isZeroPage (addr) {
  return addr.length === 2
}

function parseTiming (insn) {
  let m
  const i = insn.toLowerCase()
  if (m = /(?:lda|ldx|ldy) \$(.*)/.exec(i)) {
    return isZeroPage(m[1]) ? [R, R, R] : [R, R, R, RW]
  }
  if (m = /(?:lda|ldx|ldy) \#/.exec(i)) {
    return [R, R]
  }
  if (m = /(?:sta|stx|sty) \$(.*)/.exec(i)) {
    return isZeroPage(m[1]) ? [R, R, RW] : [R, R, R, RW]
  }
  if (m = /(?:inc|dec|asl|lsr) \$(.*)/.exec(i)) {
    return isZeroPage(m[1]) ? [R, R, R, R, W] : [R, R, R, R, R, W]
  }
  if (m = /bit \$(.*)/.exec(i)) {
    return isZeroPage(m[1]) ? [R, R, RW] : [R, R, R, RW]
  }
  if (m = /nop/.exec(i)) {
    return [R, R]
  }
  if (m = /(?:tax|tay|tsx|txa|txs|tya)/.exec(i)) {
    return [R, R]
  }
  if (m = /(?:inx|dex|iny|dey)/.exec(i)) {
    return [R, R]
  }
  if (m = /(?:bne|beq)/.exec(i)) {
    // TODO this is not static.  It's 2 if no branch taken, 3 if taken
    return [R, R, R]
  }

  console.warn('unsupported instruction', insn)
}

function parse (str) {
  const lines = str.split('\n')
  const res = []
  const insns = lines.forEach(line => {
    const re = /\.C:([0-9A-F]+)  ((?:[0-9A-F]+)(?: [0-9A-F]+)*)  [ ]+(.*)/;
    const m = re.exec(line)
    if (m) {
      const asm = m[3]
      res.push({
        address: m[1],
        encoded: m[2],
        asm,
        timing: parseTiming(asm)
      })
    }
  })
  return res
}

module.exports = { parse, R, W, RW };

},{}],2:[function(require,module,exports){
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

},{"./asm.js":1,"snabbdom":8,"snabbdom/h":3,"snabbdom/modules/attributes":6,"snabbdom/modules/style":7}],3:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var vnode_1 = require("./vnode");
var is = require("./is");
function addNS(data, children, sel) {
    data.ns = 'http://www.w3.org/2000/svg';
    if (sel !== 'foreignObject' && children !== undefined) {
        for (var i = 0; i < children.length; ++i) {
            var childData = children[i].data;
            if (childData !== undefined) {
                addNS(childData, children[i].children, children[i].sel);
            }
        }
    }
}
function h(sel, b, c) {
    var data = {}, children, text, i;
    if (c !== undefined) {
        data = b;
        if (is.array(c)) {
            children = c;
        }
        else if (is.primitive(c)) {
            text = c;
        }
        else if (c && c.sel) {
            children = [c];
        }
    }
    else if (b !== undefined) {
        if (is.array(b)) {
            children = b;
        }
        else if (is.primitive(b)) {
            text = b;
        }
        else if (b && b.sel) {
            children = [b];
        }
        else {
            data = b;
        }
    }
    if (is.array(children)) {
        for (i = 0; i < children.length; ++i) {
            if (is.primitive(children[i]))
                children[i] = vnode_1.vnode(undefined, undefined, undefined, children[i], undefined);
        }
    }
    if (sel[0] === 's' && sel[1] === 'v' && sel[2] === 'g' &&
        (sel.length === 3 || sel[3] === '.' || sel[3] === '#')) {
        addNS(data, children, sel);
    }
    return vnode_1.vnode(sel, data, children, text, undefined);
}
exports.h = h;
;
exports.default = h;

},{"./is":5,"./vnode":10}],4:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
function createElement(tagName) {
    return document.createElement(tagName);
}
function createElementNS(namespaceURI, qualifiedName) {
    return document.createElementNS(namespaceURI, qualifiedName);
}
function createTextNode(text) {
    return document.createTextNode(text);
}
function createComment(text) {
    return document.createComment(text);
}
function insertBefore(parentNode, newNode, referenceNode) {
    parentNode.insertBefore(newNode, referenceNode);
}
function removeChild(node, child) {
    node.removeChild(child);
}
function appendChild(node, child) {
    node.appendChild(child);
}
function parentNode(node) {
    return node.parentNode;
}
function nextSibling(node) {
    return node.nextSibling;
}
function tagName(elm) {
    return elm.tagName;
}
function setTextContent(node, text) {
    node.textContent = text;
}
function getTextContent(node) {
    return node.textContent;
}
function isElement(node) {
    return node.nodeType === 1;
}
function isText(node) {
    return node.nodeType === 3;
}
function isComment(node) {
    return node.nodeType === 8;
}
exports.htmlDomApi = {
    createElement: createElement,
    createElementNS: createElementNS,
    createTextNode: createTextNode,
    createComment: createComment,
    insertBefore: insertBefore,
    removeChild: removeChild,
    appendChild: appendChild,
    parentNode: parentNode,
    nextSibling: nextSibling,
    tagName: tagName,
    setTextContent: setTextContent,
    getTextContent: getTextContent,
    isElement: isElement,
    isText: isText,
    isComment: isComment,
};
exports.default = exports.htmlDomApi;

},{}],5:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.array = Array.isArray;
function primitive(s) {
    return typeof s === 'string' || typeof s === 'number';
}
exports.primitive = primitive;

},{}],6:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var xlinkNS = 'http://www.w3.org/1999/xlink';
var xmlNS = 'http://www.w3.org/XML/1998/namespace';
var colonChar = 58;
var xChar = 120;
function updateAttrs(oldVnode, vnode) {
    var key, elm = vnode.elm, oldAttrs = oldVnode.data.attrs, attrs = vnode.data.attrs;
    if (!oldAttrs && !attrs)
        return;
    if (oldAttrs === attrs)
        return;
    oldAttrs = oldAttrs || {};
    attrs = attrs || {};
    // update modified attributes, add new attributes
    for (key in attrs) {
        var cur = attrs[key];
        var old = oldAttrs[key];
        if (old !== cur) {
            if (cur === true) {
                elm.setAttribute(key, "");
            }
            else if (cur === false) {
                elm.removeAttribute(key);
            }
            else {
                if (key.charCodeAt(0) !== xChar) {
                    elm.setAttribute(key, cur);
                }
                else if (key.charCodeAt(3) === colonChar) {
                    // Assume xml namespace
                    elm.setAttributeNS(xmlNS, key, cur);
                }
                else if (key.charCodeAt(5) === colonChar) {
                    // Assume xlink namespace
                    elm.setAttributeNS(xlinkNS, key, cur);
                }
                else {
                    elm.setAttribute(key, cur);
                }
            }
        }
    }
    // remove removed attributes
    // use `in` operator since the previous `for` iteration uses it (.i.e. add even attributes with undefined value)
    // the other option is to remove all attributes with value == undefined
    for (key in oldAttrs) {
        if (!(key in attrs)) {
            elm.removeAttribute(key);
        }
    }
}
exports.attributesModule = { create: updateAttrs, update: updateAttrs };
exports.default = exports.attributesModule;

},{}],7:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var raf = (typeof window !== 'undefined' && window.requestAnimationFrame) || setTimeout;
var nextFrame = function (fn) { raf(function () { raf(fn); }); };
function setNextFrame(obj, prop, val) {
    nextFrame(function () { obj[prop] = val; });
}
function updateStyle(oldVnode, vnode) {
    var cur, name, elm = vnode.elm, oldStyle = oldVnode.data.style, style = vnode.data.style;
    if (!oldStyle && !style)
        return;
    if (oldStyle === style)
        return;
    oldStyle = oldStyle || {};
    style = style || {};
    var oldHasDel = 'delayed' in oldStyle;
    for (name in oldStyle) {
        if (!style[name]) {
            if (name[0] === '-' && name[1] === '-') {
                elm.style.removeProperty(name);
            }
            else {
                elm.style[name] = '';
            }
        }
    }
    for (name in style) {
        cur = style[name];
        if (name === 'delayed' && style.delayed) {
            for (var name2 in style.delayed) {
                cur = style.delayed[name2];
                if (!oldHasDel || cur !== oldStyle.delayed[name2]) {
                    setNextFrame(elm.style, name2, cur);
                }
            }
        }
        else if (name !== 'remove' && cur !== oldStyle[name]) {
            if (name[0] === '-' && name[1] === '-') {
                elm.style.setProperty(name, cur);
            }
            else {
                elm.style[name] = cur;
            }
        }
    }
}
function applyDestroyStyle(vnode) {
    var style, name, elm = vnode.elm, s = vnode.data.style;
    if (!s || !(style = s.destroy))
        return;
    for (name in style) {
        elm.style[name] = style[name];
    }
}
function applyRemoveStyle(vnode, rm) {
    var s = vnode.data.style;
    if (!s || !s.remove) {
        rm();
        return;
    }
    var name, elm = vnode.elm, i = 0, compStyle, style = s.remove, amount = 0, applied = [];
    for (name in style) {
        applied.push(name);
        elm.style[name] = style[name];
    }
    compStyle = getComputedStyle(elm);
    var props = compStyle['transition-property'].split(', ');
    for (; i < props.length; ++i) {
        if (applied.indexOf(props[i]) !== -1)
            amount++;
    }
    elm.addEventListener('transitionend', function (ev) {
        if (ev.target === elm)
            --amount;
        if (amount === 0)
            rm();
    });
}
exports.styleModule = {
    create: updateStyle,
    update: updateStyle,
    destroy: applyDestroyStyle,
    remove: applyRemoveStyle
};
exports.default = exports.styleModule;

},{}],8:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var vnode_1 = require("./vnode");
var is = require("./is");
var htmldomapi_1 = require("./htmldomapi");
function isUndef(s) { return s === undefined; }
function isDef(s) { return s !== undefined; }
var emptyNode = vnode_1.default('', {}, [], undefined, undefined);
function sameVnode(vnode1, vnode2) {
    return vnode1.key === vnode2.key && vnode1.sel === vnode2.sel;
}
function isVnode(vnode) {
    return vnode.sel !== undefined;
}
function createKeyToOldIdx(children, beginIdx, endIdx) {
    var i, map = {}, key, ch;
    for (i = beginIdx; i <= endIdx; ++i) {
        ch = children[i];
        if (ch != null) {
            key = ch.key;
            if (key !== undefined)
                map[key] = i;
        }
    }
    return map;
}
var hooks = ['create', 'update', 'remove', 'destroy', 'pre', 'post'];
var h_1 = require("./h");
exports.h = h_1.h;
var thunk_1 = require("./thunk");
exports.thunk = thunk_1.thunk;
function init(modules, domApi) {
    var i, j, cbs = {};
    var api = domApi !== undefined ? domApi : htmldomapi_1.default;
    for (i = 0; i < hooks.length; ++i) {
        cbs[hooks[i]] = [];
        for (j = 0; j < modules.length; ++j) {
            var hook = modules[j][hooks[i]];
            if (hook !== undefined) {
                cbs[hooks[i]].push(hook);
            }
        }
    }
    function emptyNodeAt(elm) {
        var id = elm.id ? '#' + elm.id : '';
        var c = elm.className ? '.' + elm.className.split(' ').join('.') : '';
        return vnode_1.default(api.tagName(elm).toLowerCase() + id + c, {}, [], undefined, elm);
    }
    function createRmCb(childElm, listeners) {
        return function rmCb() {
            if (--listeners === 0) {
                var parent_1 = api.parentNode(childElm);
                api.removeChild(parent_1, childElm);
            }
        };
    }
    function createElm(vnode, insertedVnodeQueue) {
        var i, data = vnode.data;
        if (data !== undefined) {
            if (isDef(i = data.hook) && isDef(i = i.init)) {
                i(vnode);
                data = vnode.data;
            }
        }
        var children = vnode.children, sel = vnode.sel;
        if (sel === '!') {
            if (isUndef(vnode.text)) {
                vnode.text = '';
            }
            vnode.elm = api.createComment(vnode.text);
        }
        else if (sel !== undefined) {
            // Parse selector
            var hashIdx = sel.indexOf('#');
            var dotIdx = sel.indexOf('.', hashIdx);
            var hash = hashIdx > 0 ? hashIdx : sel.length;
            var dot = dotIdx > 0 ? dotIdx : sel.length;
            var tag = hashIdx !== -1 || dotIdx !== -1 ? sel.slice(0, Math.min(hash, dot)) : sel;
            var elm = vnode.elm = isDef(data) && isDef(i = data.ns) ? api.createElementNS(i, tag)
                : api.createElement(tag);
            if (hash < dot)
                elm.setAttribute('id', sel.slice(hash + 1, dot));
            if (dotIdx > 0)
                elm.setAttribute('class', sel.slice(dot + 1).replace(/\./g, ' '));
            for (i = 0; i < cbs.create.length; ++i)
                cbs.create[i](emptyNode, vnode);
            if (is.array(children)) {
                for (i = 0; i < children.length; ++i) {
                    var ch = children[i];
                    if (ch != null) {
                        api.appendChild(elm, createElm(ch, insertedVnodeQueue));
                    }
                }
            }
            else if (is.primitive(vnode.text)) {
                api.appendChild(elm, api.createTextNode(vnode.text));
            }
            i = vnode.data.hook; // Reuse variable
            if (isDef(i)) {
                if (i.create)
                    i.create(emptyNode, vnode);
                if (i.insert)
                    insertedVnodeQueue.push(vnode);
            }
        }
        else {
            vnode.elm = api.createTextNode(vnode.text);
        }
        return vnode.elm;
    }
    function addVnodes(parentElm, before, vnodes, startIdx, endIdx, insertedVnodeQueue) {
        for (; startIdx <= endIdx; ++startIdx) {
            var ch = vnodes[startIdx];
            if (ch != null) {
                api.insertBefore(parentElm, createElm(ch, insertedVnodeQueue), before);
            }
        }
    }
    function invokeDestroyHook(vnode) {
        var i, j, data = vnode.data;
        if (data !== undefined) {
            if (isDef(i = data.hook) && isDef(i = i.destroy))
                i(vnode);
            for (i = 0; i < cbs.destroy.length; ++i)
                cbs.destroy[i](vnode);
            if (vnode.children !== undefined) {
                for (j = 0; j < vnode.children.length; ++j) {
                    i = vnode.children[j];
                    if (i != null && typeof i !== "string") {
                        invokeDestroyHook(i);
                    }
                }
            }
        }
    }
    function removeVnodes(parentElm, vnodes, startIdx, endIdx) {
        for (; startIdx <= endIdx; ++startIdx) {
            var i_1 = void 0, listeners = void 0, rm = void 0, ch = vnodes[startIdx];
            if (ch != null) {
                if (isDef(ch.sel)) {
                    invokeDestroyHook(ch);
                    listeners = cbs.remove.length + 1;
                    rm = createRmCb(ch.elm, listeners);
                    for (i_1 = 0; i_1 < cbs.remove.length; ++i_1)
                        cbs.remove[i_1](ch, rm);
                    if (isDef(i_1 = ch.data) && isDef(i_1 = i_1.hook) && isDef(i_1 = i_1.remove)) {
                        i_1(ch, rm);
                    }
                    else {
                        rm();
                    }
                }
                else {
                    api.removeChild(parentElm, ch.elm);
                }
            }
        }
    }
    function updateChildren(parentElm, oldCh, newCh, insertedVnodeQueue) {
        var oldStartIdx = 0, newStartIdx = 0;
        var oldEndIdx = oldCh.length - 1;
        var oldStartVnode = oldCh[0];
        var oldEndVnode = oldCh[oldEndIdx];
        var newEndIdx = newCh.length - 1;
        var newStartVnode = newCh[0];
        var newEndVnode = newCh[newEndIdx];
        var oldKeyToIdx;
        var idxInOld;
        var elmToMove;
        var before;
        while (oldStartIdx <= oldEndIdx && newStartIdx <= newEndIdx) {
            if (oldStartVnode == null) {
                oldStartVnode = oldCh[++oldStartIdx]; // Vnode might have been moved left
            }
            else if (oldEndVnode == null) {
                oldEndVnode = oldCh[--oldEndIdx];
            }
            else if (newStartVnode == null) {
                newStartVnode = newCh[++newStartIdx];
            }
            else if (newEndVnode == null) {
                newEndVnode = newCh[--newEndIdx];
            }
            else if (sameVnode(oldStartVnode, newStartVnode)) {
                patchVnode(oldStartVnode, newStartVnode, insertedVnodeQueue);
                oldStartVnode = oldCh[++oldStartIdx];
                newStartVnode = newCh[++newStartIdx];
            }
            else if (sameVnode(oldEndVnode, newEndVnode)) {
                patchVnode(oldEndVnode, newEndVnode, insertedVnodeQueue);
                oldEndVnode = oldCh[--oldEndIdx];
                newEndVnode = newCh[--newEndIdx];
            }
            else if (sameVnode(oldStartVnode, newEndVnode)) {
                patchVnode(oldStartVnode, newEndVnode, insertedVnodeQueue);
                api.insertBefore(parentElm, oldStartVnode.elm, api.nextSibling(oldEndVnode.elm));
                oldStartVnode = oldCh[++oldStartIdx];
                newEndVnode = newCh[--newEndIdx];
            }
            else if (sameVnode(oldEndVnode, newStartVnode)) {
                patchVnode(oldEndVnode, newStartVnode, insertedVnodeQueue);
                api.insertBefore(parentElm, oldEndVnode.elm, oldStartVnode.elm);
                oldEndVnode = oldCh[--oldEndIdx];
                newStartVnode = newCh[++newStartIdx];
            }
            else {
                if (oldKeyToIdx === undefined) {
                    oldKeyToIdx = createKeyToOldIdx(oldCh, oldStartIdx, oldEndIdx);
                }
                idxInOld = oldKeyToIdx[newStartVnode.key];
                if (isUndef(idxInOld)) {
                    api.insertBefore(parentElm, createElm(newStartVnode, insertedVnodeQueue), oldStartVnode.elm);
                    newStartVnode = newCh[++newStartIdx];
                }
                else {
                    elmToMove = oldCh[idxInOld];
                    if (elmToMove.sel !== newStartVnode.sel) {
                        api.insertBefore(parentElm, createElm(newStartVnode, insertedVnodeQueue), oldStartVnode.elm);
                    }
                    else {
                        patchVnode(elmToMove, newStartVnode, insertedVnodeQueue);
                        oldCh[idxInOld] = undefined;
                        api.insertBefore(parentElm, elmToMove.elm, oldStartVnode.elm);
                    }
                    newStartVnode = newCh[++newStartIdx];
                }
            }
        }
        if (oldStartIdx <= oldEndIdx || newStartIdx <= newEndIdx) {
            if (oldStartIdx > oldEndIdx) {
                before = newCh[newEndIdx + 1] == null ? null : newCh[newEndIdx + 1].elm;
                addVnodes(parentElm, before, newCh, newStartIdx, newEndIdx, insertedVnodeQueue);
            }
            else {
                removeVnodes(parentElm, oldCh, oldStartIdx, oldEndIdx);
            }
        }
    }
    function patchVnode(oldVnode, vnode, insertedVnodeQueue) {
        var i, hook;
        if (isDef(i = vnode.data) && isDef(hook = i.hook) && isDef(i = hook.prepatch)) {
            i(oldVnode, vnode);
        }
        var elm = vnode.elm = oldVnode.elm;
        var oldCh = oldVnode.children;
        var ch = vnode.children;
        if (oldVnode === vnode)
            return;
        if (vnode.data !== undefined) {
            for (i = 0; i < cbs.update.length; ++i)
                cbs.update[i](oldVnode, vnode);
            i = vnode.data.hook;
            if (isDef(i) && isDef(i = i.update))
                i(oldVnode, vnode);
        }
        if (isUndef(vnode.text)) {
            if (isDef(oldCh) && isDef(ch)) {
                if (oldCh !== ch)
                    updateChildren(elm, oldCh, ch, insertedVnodeQueue);
            }
            else if (isDef(ch)) {
                if (isDef(oldVnode.text))
                    api.setTextContent(elm, '');
                addVnodes(elm, null, ch, 0, ch.length - 1, insertedVnodeQueue);
            }
            else if (isDef(oldCh)) {
                removeVnodes(elm, oldCh, 0, oldCh.length - 1);
            }
            else if (isDef(oldVnode.text)) {
                api.setTextContent(elm, '');
            }
        }
        else if (oldVnode.text !== vnode.text) {
            api.setTextContent(elm, vnode.text);
        }
        if (isDef(hook) && isDef(i = hook.postpatch)) {
            i(oldVnode, vnode);
        }
    }
    return function patch(oldVnode, vnode) {
        var i, elm, parent;
        var insertedVnodeQueue = [];
        for (i = 0; i < cbs.pre.length; ++i)
            cbs.pre[i]();
        if (!isVnode(oldVnode)) {
            oldVnode = emptyNodeAt(oldVnode);
        }
        if (sameVnode(oldVnode, vnode)) {
            patchVnode(oldVnode, vnode, insertedVnodeQueue);
        }
        else {
            elm = oldVnode.elm;
            parent = api.parentNode(elm);
            createElm(vnode, insertedVnodeQueue);
            if (parent !== null) {
                api.insertBefore(parent, vnode.elm, api.nextSibling(elm));
                removeVnodes(parent, [oldVnode], 0, 0);
            }
        }
        for (i = 0; i < insertedVnodeQueue.length; ++i) {
            insertedVnodeQueue[i].data.hook.insert(insertedVnodeQueue[i]);
        }
        for (i = 0; i < cbs.post.length; ++i)
            cbs.post[i]();
        return vnode;
    };
}
exports.init = init;

},{"./h":3,"./htmldomapi":4,"./is":5,"./thunk":9,"./vnode":10}],9:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var h_1 = require("./h");
function copyToThunk(vnode, thunk) {
    thunk.elm = vnode.elm;
    vnode.data.fn = thunk.data.fn;
    vnode.data.args = thunk.data.args;
    thunk.data = vnode.data;
    thunk.children = vnode.children;
    thunk.text = vnode.text;
    thunk.elm = vnode.elm;
}
function init(thunk) {
    var cur = thunk.data;
    var vnode = cur.fn.apply(undefined, cur.args);
    copyToThunk(vnode, thunk);
}
function prepatch(oldVnode, thunk) {
    var i, old = oldVnode.data, cur = thunk.data;
    var oldArgs = old.args, args = cur.args;
    if (old.fn !== cur.fn || oldArgs.length !== args.length) {
        copyToThunk(cur.fn.apply(undefined, args), thunk);
        return;
    }
    for (i = 0; i < args.length; ++i) {
        if (oldArgs[i] !== args[i]) {
            copyToThunk(cur.fn.apply(undefined, args), thunk);
            return;
        }
    }
    copyToThunk(oldVnode, thunk);
}
exports.thunk = function thunk(sel, key, fn, args) {
    if (args === undefined) {
        args = fn;
        fn = key;
        key = undefined;
    }
    return h_1.h(sel, {
        key: key,
        hook: { init: init, prepatch: prepatch },
        fn: fn,
        args: args
    });
};
exports.default = exports.thunk;

},{"./h":3}],10:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
function vnode(sel, data, children, text, elm) {
    var key = data === undefined ? undefined : data.key;
    return { sel: sel, data: data, children: children,
        text: text, elm: elm, key: key };
}
exports.vnode = vnode;
exports.default = vnode;

},{}]},{},[2])(2)
});
