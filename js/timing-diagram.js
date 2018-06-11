var snabbdom = require('snabbdom');
var patch = snabbdom.init([ // Init patch function with chosen modules
  require('snabbdom/modules/attributes').default, // makes it easy to toggle classes
]);
var h = require('snabbdom/h').default; // helper function for creating vnodes

class TimingDiagram {
  constructor () {
    this.vnode = null
    this.theta = 0.0
  }

  view () {
    var view = h('div', [
      h('svg', {attrs: {width: 100, height: 100}}, [
        h('circle', {attrs: {cx: 50, cy: 50, r: Math.sin(this.theta)*10.0+40, fill: 'blue'}})
      ])
    ])
    return view
  }

  render () {
    patch(this.vnode, this.view())
  }

  mount (container) {
    this.vnode = patch(container, this.view())

    setInterval(cb => {
      this.render()
      this.theta += 0.1
    }, 100)
  }
}

module.exports = { TimingDiagram };
