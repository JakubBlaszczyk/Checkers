package com.pk.database;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.LinkedList;
import java.util.List;

public class Database implements DatabaseAccess {

  public static final String DRIVER = "org.sqlite.JDBC";
  public static final String DB_URL = "jdbc:sqlite:./client/target/database.db";

  private Connection conn;
  private Statement stat;

  public Database() {
    try {
      Class.forName(Database.DRIVER);
    } catch (ClassNotFoundException e) {
      System.err.println("Missing driver JDBC");
      e.printStackTrace();
    }

    try {
      conn = DriverManager.getConnection(DB_URL);
      stat = conn.createStatement();
    } catch (SQLException e) {
      System.err.println("Connection initialization problem");
      e.printStackTrace();
    }

    createTables();
  }

  public boolean createTables() {
    String creategame = "CREATE TABLE IF NOT EXISTS Game (id INTEGER PRIMARY KEY AUTOINCREMENT, player1 varchar(20), player2 varchar(20))";
    String createmapHistory = "CREATE TABLE IF NOT EXISTS MapHistory (gameId INTEGER PRIMARY KEY, step int(3), stepBefore varchar(4), stepAfter varchar(4))";
    //asd
    try {
      stat.execute(creategame);

      stat.execute(createmapHistory);
    } catch (SQLException e) {
      System.err.println("Create table error");
      e.printStackTrace();
      return false;
    }
    return true;
  }

  public boolean insertIntoGame(String player1, String player2) {
    try {
      PreparedStatement prepStmt = conn.prepareStatement("insert into Game values (NULL, ?, ?);");
      prepStmt.setString(1, player1);
      prepStmt.setString(2, player2);
      prepStmt.execute();
    } catch (SQLException e) {
      System.err.println("Insert Game error");
      e.printStackTrace();
      return false;
    }
    return true;
  }

  public boolean insertIntoMapHistory(int gameId, int step, String stepBefore, String stepAfter) {
    try {
      PreparedStatement prepStmt = conn.prepareStatement("insert into MapHistory values (?, ?, ?, ?);");
      prepStmt.setInt(0, gameId);
      prepStmt.setInt(1, step);
      prepStmt.setString(2, stepBefore);
      prepStmt.setString(3, stepAfter);
      prepStmt.execute();
    } catch (SQLException e) {
      System.err.println("Insert MapHistory error");
      e.printStackTrace();
      return false;
    }
    return true;
  }

  public List<Game> selectFromGame() {
    List<Game> Game = new LinkedList<>();
    try {
      ResultSet result = stat.executeQuery("SELECT * FROM Game");
      int gameId;
      String player1, player2;
      while (result.next()) {
        gameId = result.getInt("id");
        player1 = result.getString("player1");
        player2 = result.getString("player2");
        Game.add(new Game(gameId, player1, player2));
      }
    } catch (SQLException e) {
      e.printStackTrace();
      return null;
    }
    return Game;
  }

  public List<MapHistory> selectFromMapHistory() {
    List<MapHistory> MapHistory = new LinkedList<>();
    try {
      ResultSet result = stat.executeQuery("SELECT * FROM MapHistory");
      int id, step;
      String stepBefore, stepAfter;
      while (result.next()) {
        id = result.getInt("gameId");
        step = result.getInt("step");
        stepBefore = result.getString("stepBefore");
        stepAfter = result.getString("stepAfter");
        MapHistory.add(new MapHistory(id, step, stepBefore, stepAfter));
      }
    } catch (SQLException e) {
      e.printStackTrace();
      return null;
    }
    return MapHistory;
  }

  public void closeConnection() {
    try {
      conn.close();
    } catch (SQLException e) {
      System.err.println("Close connection error");
      e.printStackTrace();
    }
  }
}
