package com.pk.database;

import lombok.Value;

@Value
public class MapHistory {
  int gameId;
  int step;
  int stepBefore;
  int stepAfter;
}
