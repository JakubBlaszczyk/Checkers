package com.pk.database;

import lombok.Value;

@Value
public class MapHistory {
  int id;
  int gameId;
  int step;
  int xBefore;
  int yBefore;
  int xAfter;
  int yAfter;

  @Override
  public String toString() {
    return "Step " + step + ": [" + xBefore + ", " + yBefore + "] => [" + xAfter + ", " + yAfter + "]";
  }
}
