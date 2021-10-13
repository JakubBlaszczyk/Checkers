package com.pk.server.models;

import lombok.Value;

@Value
public class Move {
  Integer srcX;
  Integer srcY;
  Integer dstX;
  Integer dstY;

  public String toSendableFormat() {
    return String.valueOf(srcX) + " " + srcY + " " + dstX + " " + dstY;
  }
}
