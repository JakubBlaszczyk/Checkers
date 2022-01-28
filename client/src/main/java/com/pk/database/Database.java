package com.pk.database;

import lombok.extern.slf4j.Slf4j;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.LinkedList;
import java.util.List;

@Slf4j
public class Database implements DatabaseAccess {

  public static final String DRIVER = "org.sqlite.JDBC";
  public static final String DB_URL = "jdbc:sqlite:./";

  private Connection conn;
  private Statement stat;

  public Database(String dbName) throws SQLException {
    try {
      Class.forName(Database.DRIVER);
    } catch (ClassNotFoundException e) {
      log.error("Missing driver JDBC");
      throw new SQLException(e);
    }

    try {
      conn = DriverManager.getConnection(DB_URL + dbName);
      stat = conn.createStatement();
    } catch (SQLException e) {
      log.error("Connection initialization problem");
      throw new SQLException(e);
    }
    createTables();
  }

  public boolean createTables() {
    String createGame = "CREATE TABLE IF NOT EXISTS Game (id INTEGER PRIMARY KEY AUTOINCREMENT, player1 varchar(20), player2 varchar(20))";
    String createMapHistory = "CREATE TABLE IF NOT EXISTS MapHistory (gameId INTEGER, step int(3), xBefore int(2), yBefore int(2), xAfter int(2), yAfter int(2), PRIMARY KEY (gameID, step) ,FOREIGN KEY(gameID) REFERENCES Game(id))";
    try {
      stat.execute(createGame);

      stat.execute(createMapHistory);
    } catch (SQLException e) {
      System.err.println("Create table error");
      e.printStackTrace();
      return false;
    }
    return true;
  }

  public int insertIntoGame(String player1, String player2) {
    try {
      PreparedStatement prepStmt = conn.prepareStatement("insert into Game values (NULL, ?, ?);");
      prepStmt.setString(1, player1);
      prepStmt.setString(2, player2);
      prepStmt.execute();
      try (ResultSet key = prepStmt.getGeneratedKeys()) {
        if (key.next()) {
          return key.getInt(1);
        }
      }
    } catch (SQLException e) {
      System.err.println("Insert Game error");
      e.printStackTrace();
    }
    return -1;
  }

  public boolean insertIntoMapHistory(int gameId, int xBefore, int yBefore, int xAfter, int yAfter) {
    int step = selectStepFromMapHistory(gameId);
    log.debug("{}", step);
    try {
      PreparedStatement prepStmt = conn.prepareStatement("insert into MapHistory values (?, ?, ?, ?, ?, ?)");
      prepStmt.setInt(1, gameId);
      prepStmt.setInt(2, step);
      prepStmt.setInt(3, xBefore);
      prepStmt.setInt(4, yBefore);
      prepStmt.setInt(5, xAfter);
      prepStmt.setInt(6, yAfter);
      prepStmt.execute();
    } catch (SQLException e) {
      System.err.println("Insert MapHistory error: Something is wrong my friend");
      e.printStackTrace();
      return false;
    }
    return true;
  }

  public List<Game> selectFromGame() {
    List<Game> game = new LinkedList<>();
    try {
      ResultSet result = stat.executeQuery("SELECT * FROM Game");
      int gameId;
      String player1, player2;
      while (result.next()) {
        gameId = result.getInt("id");
        player1 = result.getString("player1");
        player2 = result.getString("player2");
        game.add(new Game(gameId, player1, player2));
      }
    } catch (SQLException e) {
      e.printStackTrace();
      return null;
    }
    return game;
  }

  public int selectStepFromMapHistory(int gameId) {
    int step = 0;
    try {
      ResultSet result = stat.executeQuery("SELECT step FROM MapHistory WHERE gameId = " + gameId + " ORDER BY step DESC limit 1");
      while (result.next()) {
        step = result.getInt("step");
      }
    } catch (SQLException e) {
      e.printStackTrace();
      return 1;
    }
    return step + 1;
  }

  public List<MapHistory> selectFromMapHistory(int gameId) {
    List<MapHistory> mapHistory = new LinkedList<>();
    try {
      ResultSet result = stat.executeQuery("SELECT * FROM MapHistory WHERE gameId = " + gameId);
      int id, step, xBefore, yBefore, xAfter, yAfter;
      while (result.next()) {
        id = result.getInt("gameId");
        step = result.getInt("step");
        gameId = result.getInt("gameId");
        xBefore = result.getInt("xBefore");
        yBefore = result.getInt("yBefore");
        xAfter = result.getInt("xAfter");
        yAfter = result.getInt("yAfter");
        mapHistory.add(new MapHistory(id, gameId, step, xBefore, yBefore, xAfter, yAfter));
      }
    } catch (SQLException e) {
      e.printStackTrace();
      return null;
    }
    return mapHistory;
  }

  public void closeConnection() {
    try {
      conn.close();
    } catch (SQLException e) {
      System.err.println("Close connection error: Its still open bro");
      e.printStackTrace();
    }
  }
}
