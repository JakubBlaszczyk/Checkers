package com.pk.database;

import java.util.List;

public interface DatabaseAccess {

  public boolean insertIntoGame(String player1, String player2);

  public boolean insertIntoMapHistory(int gameId, int step, String stepBefore, String stepAfter);

  public List<Game> selectFromGame();

  public List<MapHistory> selectFromMapHistory();

  public void closeConnection();

}
