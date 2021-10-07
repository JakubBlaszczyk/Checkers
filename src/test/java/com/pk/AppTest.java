package com.pk;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.List;

import com.pk.logic.BasicLogic;
import com.pk.logic.Logic;

import org.junit.Test;

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

  @Test
  public void initializeLogicBoard() {
    List<Integer> exampleSmallBoard = new ArrayList<Integer>(3);
    exampleSmallBoard.add(2);
    exampleSmallBoard.add(1);
    exampleSmallBoard.add(4);
    List<List<Integer>> exampleBigBoard = new ArrayList<>(3);
    exampleBigBoard.add(exampleSmallBoard);
    exampleBigBoard.add(exampleSmallBoard);
    exampleBigBoard.add(exampleSmallBoard);
    Logic finalLogic = new BasicLogic(exampleBigBoard);
    assertEquals("214\n214\n214\n", finalLogic.toString());
  }

  public @Test void assertFalse() {
    assertTrue(false);
  }
}
