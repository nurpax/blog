
const R  = 'R'
const W  = 'W'
const RW = 'RW'

function isZeroPage (addr) {
  return addr.length === 2
}

function parseTiming (insn) {
  var m;
  const i = insn.toLowerCase()
  if (m = /(?:lda|ldx|ldy) (?:\$|\.)(.*)/.exec(i)) {
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
    const re = /\.C:([0-9a-fA-F]+)  ((?:[0-9A-F]+)(?: [0-9A-F]+)*)  [ ]+(.*)/;
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
