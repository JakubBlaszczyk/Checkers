package com.pk;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.List;

import com.pk.logic.BasicLogic;
import com.pk.logic.Logic;
import com.pk.logic.Piece;

import org.junit.jupiter.api.Test;

/**
 * Unit test for simple App.
 */
public class AppTest {
  /**
   * Rigorous Test :-)
   */
  @Test
  public void shouldAnswerWithTrue() {
    assertTrue(true);
  }

  @org.junit.jupiter.api.Test
  public void initializeLogicBoard() {
    List<Piece> exampleSmallBoard = new ArrayList<>(3);
    exampleSmallBoard.add(Piece.WHITE_PAWN);
    exampleSmallBoard.add(Piece.BLACK_PAWN);
    exampleSmallBoard.add(Piece.BLACK_PAWN);
    List<List<Piece>> exampleBigBoard = new ArrayList<>(3);
    exampleBigBoard.add(exampleSmallBoard);
    exampleBigBoard.add(exampleSmallBoard);
    exampleBigBoard.add(exampleSmallBoard);
    Logic finalLogic = new BasicLogic(exampleBigBoard);
    assertEquals("WBB\nWBB\nWBB\n", finalLogic.toString());
  }
}
