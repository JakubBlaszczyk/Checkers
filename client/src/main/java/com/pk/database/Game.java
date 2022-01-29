package com.pk.database;

import lombok.Value;

@Value
public class Game {
  int id;
  String player1;
  String player2;

  @Override
  public String toString() {
    return "Game(id=" + id + ", player1=" + player1 + ", player2=" + player2 + ")";
  }
}
