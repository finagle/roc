package roc
package postgresql
package transport

import java.nio.charset.StandardCharsets

abstract class BufferTestData {
  val TestInt = 71
  val TestString = "␂䤥됳ⲟ鮩躾へ囙엊ꡓ쿟䏾뫱㳫䕣찵䥵총몫Ғ칆縏洰둈Ã糧舝裂ꚦ鯅晔뭿栛鍯楾ꑜ辔ᖊ쐡퍽䌡鏹탪ຣ剻䅧୷㒒凹ᱣ浤⯵ϵ蒤ꘞ觲᜴씻꠩ಯ힭ཷ秞囬榙ꥲ确텳꩞邀ḷ菲裁劋謆凰겕렶謱鴆◛Ⱬı熳샹칻"
  val TestStringByteLength = TestString.getBytes(StandardCharsets.UTF_8).length + 1
}
