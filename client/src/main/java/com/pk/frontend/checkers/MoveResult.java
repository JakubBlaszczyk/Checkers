package com.pk.frontend.checkers;

import com.pk.logic.Indices;
import lombok.Value;

@Value
public class MoveResult {

  MoveType type;
  Indices indices;

  public MoveResult(MoveType type) {
    this(type, null);
  }

  public MoveResult(MoveType type, Indices indices) {
    this.type = type;
    this.indices = indices;
  }
}
