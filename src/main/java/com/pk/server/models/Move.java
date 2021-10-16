package com.pk.server.models;

import java.io.IOException;

import lombok.Value;
import lombok.extern.slf4j.Slf4j;

@Value
@Slf4j
public class Move {
  Integer srcX;
  Integer srcY;
  Integer dstX;
  Integer dstY;

  public String toSendableFormat() {
    String msg = String.valueOf(srcX) + " " + srcY + " " + dstX + " " + dstY; 
    log.info("toSendableFormat() -> " + msg);
    return msg;
  }

  public static Move fromString(String msg) throws IOException {
    String[] tokens = msg.split(" ");
    if (tokens.length != 4) {
      log.error("Invalid string passed, tokens len: " + tokens.length);
      throw new IOException("placeholder");
    }
    return new Move(Integer.valueOf(tokens[0]), Integer.valueOf(tokens[1]), Integer.valueOf(tokens[2]), Integer.valueOf(tokens[3]));
  }
}
