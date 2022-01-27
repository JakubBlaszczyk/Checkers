package com.pk.database;


import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;

import java.sql.SQLException;
import java.util.List;


public class test {

  Database b = null;;

  @Test
  @BeforeEach
  public void openDatabase() {
    try {
      b = new Database("test.db");
    } catch (SQLException e) {
      e.printStackTrace();
    }
  }

  @Test
  @AfterEach
  public void closeDatabase() {
    b.closeConnection();
  }

  @Test
  public void createGameRecord() {
    b.insertIntoGame("Arek", "Daniel");
    b.insertIntoGame("Jan", "Jakub");

    List<Game> matches = b.selectFromGame();

    System.out.println("Lista meczów: ");
    for (Game c : matches)
      System.out.println(c);
  }

  @Test
  public void createMapHistoryRecords() {
    b.insertIntoMapHistory(1, 1, 1, 2, 2);
    b.insertIntoMapHistory(1, 2, 2, 3, 3);
    b.insertIntoMapHistory(1, 3, 3, 4, 4);
    b.insertIntoMapHistory(1, 4, 4, 5, 5);

    b.insertIntoMapHistory(2, 1, 1, 2, 2);
    b.insertIntoMapHistory(2, 2, 2, 3, 3);
    b.insertIntoMapHistory(2, 3, 3, 4, 4);
    b.insertIntoMapHistory(2, 4, 4, 5, 5);

    List<MapHistory> steps = b.selectFromMapHistory(1);

    System.out.println("Kroków lista: ");
    for (MapHistory s : steps)
      System.out.println(s);
  }

  @Test
  public void selectFromMapHistory() {
    List<MapHistory> steps = b.selectFromMapHistory(1);

    System.out.println("Kroków lista: ");
    for (MapHistory s : steps)
      System.out.println(s);

  }
}
