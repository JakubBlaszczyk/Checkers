package com.pk.database;

import java.util.List;

public interface DatabaseAccess {

  public boolean insertIntoGame(String player1, String player2);

  public boolean insertIntoMapHistory(int gameId, int xBefore, int yBefore, int xAfter, int yAfter);

  public List<Game> selectFromGame();

  public List<MapHistory> selectFromMapHistory(int gameId);

  public void closeConnection();

}
