package com.pk.database;

public class Game {
  private int id;
  private String player1;
  private String player2;

  public int getId() {
    return id;
  }

  public void setId(int id) {
    this.id = id;
  }

  public String getPlayer1() {
    return player1;
  }

  public void setPlayer1(String player1) {
    this.player1 = player1;
  }

  public String getPlayer2() {
    return player2;
  }

  public void setPlayer2(String player2) {
    this.player2 = player2;
  }

  public Game() {
  }

  public Game(int id, String player1, String player2) {
    this.id = id;
    this.player1 = player1;
    this.player2 = player2;
  }

  public String toString() {
    return "[" + id + "] - " + player1 + " - " + player2;
  }
}
