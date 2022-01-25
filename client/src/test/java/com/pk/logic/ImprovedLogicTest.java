package com.pk.logic;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.pk.frontend.checkers.MoveType;

import org.junit.Test;
import org.junit.jupiter.api.Order;

import lombok.extern.slf4j.Slf4j;

@Slf4j
public class ImprovedLogicTest {

  @Order(0)
  @Test
  public void checkBoardNormalMove() {
    try {
      ImprovedLogic logic = new ImprovedLogic(8);
      log.debug("NONE");
      assertEquals(MoveType.NONE, logic.update(0, 0, 1, 1));
      log.debug("NONE");
      assertEquals(MoveType.NONE, logic.update(2, 1, 1, 1));
      log.debug("NORMAL");
      assertEquals(MoveType.NORMAL, logic.update(2, 2, 1, 1));
      log.debug("NONE");
      assertEquals(MoveType.NONE, logic.update(2, 2, 1, 1));
      log.debug("NORMAL");
      assertEquals(MoveType.NORMAL, logic.update(5, 5, 6, 6));
      log.debug("NORMAL");
      assertEquals(MoveType.NORMAL, logic.update(3, 3, 2, 2));
      log.debug("NONE");
      assertEquals(MoveType.NONE, logic.update(3, 3, 2, 2));
      log.debug("NORMAL");
      assertEquals(MoveType.NORMAL, logic.update(4, 4, 5, 5));
      log.debug("KILL");
      assertEquals(MoveType.KILL, logic.update(5, 5, 3, 3));
    } catch (Exception e) {
      log.error("Messegu", e);
    }
  }
}
