package com.buzuli.advent.days

import com.buzuli.UnitSpec
import com.buzuli.advent.days.day8.Message

class day8spec extends UnitSpec {
  "day8" when {
    "decoding a message" should {
      "translate strings to digits" in {
        assert(Message("gabfed acbdfg cd gebcd gdfecb bgfdcae ecdf dcb gdbfe gaceb | dcb dgfceab cbfdeg edcf").decodeMap.size == 6)
      }

      "translate messages to counts" in {
        assert(Message("bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef").decodedValue == 1625)
        assert(Message("be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe").decodedValue == 8394)
        assert(Message("edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc").decodedValue == 9781)
        assert(Message("fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg").decodedValue == 1197)
        assert(Message("fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb").decodedValue == 9361)
        assert(Message("aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea").decodedValue == 4873)
        assert(Message("fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb").decodedValue == 8418)
        assert(Message("dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe").decodedValue == 4548)
        assert(Message("egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb").decodedValue == 8717)
        assert(Message("gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce").decodedValue == 4315)
      }
    }
  }
}
