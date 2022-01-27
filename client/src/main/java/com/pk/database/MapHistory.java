package com.pk.database;

import lombok.AllArgsConstructor;
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
}