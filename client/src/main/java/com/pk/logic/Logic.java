package com.pk.logic;

import java.util.List;

import com.pk.logic.exceptions.MandatoryKillMove;
import com.pk.logic.exceptions.MoreThanOneMoveMade;
import com.pk.logic.exceptions.MoveOnAlreadyTakenSpace;
import com.pk.logic.exceptions.VerticalOrHorizontalMove;

public interface Logic {
    /**
     * This method updates board variable and throws if any illegal move occured.
     * Otherwise method doesn't throw anything. Passed boards must contain only one
     * alteration of position (one tick).
     * 
     * @throws MandatoryKillMove
     * @throws VerticalOrHorizontalMove
     * 
     * @param board next state of a board
     */
    public void update(List<List<Piece>> board) throws MandatoryKillMove, VerticalOrHorizontalMove, MoreThanOneMoveMade, MoveOnAlreadyTakenSpace;
}
