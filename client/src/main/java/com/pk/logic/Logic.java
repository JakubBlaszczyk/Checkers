package com.pk.logic;

import com.pk.frontend.checkers.MoveResult;

public interface Logic {

  /**
   * Method determines what type of move is to be taken with passed coordinates
   * 
   * @param newX it is just offset if move is left or right, coordinate for pawn
   *             to have after move
   * @param newY it is important as it is used to calculate direction in which
   *             pawn is moving, if mistaken will result in NONE; coordinate for
   *             pawn to have after move
   * @param oldX coordinate of pawn before move for X axis
   * @param oldY coordinate of pawn before move for Y axis; very important value
   *             that is used in determining direction
   * @return NONE if no move is to be made; MOVE if move is correctly taken; KILL
   *         if there is need for second pawn to be modified, it also returns
   *         indices of piece to be removed; MANDATORY_KILL if only kill move can
   *         be made and error is to be shown
   */
  public MoveResult update(Integer newX, Integer newY, Integer oldX, Integer oldY);

  /**
   * @return String that is pretty board
   */
  public String toString();

}
