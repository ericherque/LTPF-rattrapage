SRC_DIR = src
LIB_DIR = includes
OBJ_DIR = obj
BIN_DIR = exec

CC = ocamlc
FLAGS = -g

all : directories main

directories : $(OBJ_DIR) $(BIN_DIR)

$(OBJ_DIR) :
	mkdir $(OBJ_DIR)

$(BIN_DIR) :
	mkdir $(BIN_DIR)
	
	
main :  $(OBJ_DIR)/parseur.cmo $(OBJ_DIR)/state.cmo $(OBJ_DIR)/config.cmo $(OBJ_DIR)/exec.cmo
	$(CC) $^ -o $(BIN_DIR)/$@
	
$(OBJ_DIR)/state.cmi : $(LIB_DIR)/state.mli $(OBJ_DIR)/parseur.cmi
	$(CC) -c $(FLAGS) $< -I $(OBJ_DIR)/ -o $@

$(OBJ_DIR)/parseur.cmi : $(LIB_DIR)/parseur.mli
	$(CC) -c $(FLAGS) $< -I $(OBJ_DIR)/ -o $@
	
$(OBJ_DIR)/config.cmi : $(LIB_DIR)/config.mli $(OBJ_DIR)/parseur.cmi $(OBJ_DIR)/state.cmi
	$(CC) -c $(FLAGS) $< -I $(OBJ_DIR)/ -o $@
	
$(OBJ_DIR)/state.cmo : $(SRC_DIR)/state.ml $(OBJ_DIR)/state.cmi $(OBJ_DIR)/parseur.cmo
	$(CC) -c $(FLAGS) $< -I $(OBJ_DIR)/ -o $@

$(OBJ_DIR)/parseur.cmo : $(SRC_DIR)/parseur.ml $(OBJ_DIR)/parseur.cmi
	$(CC) -c $(FLAGS) $< -I $(OBJ_DIR)/ -o $@

$(OBJ_DIR)/config.cmo : $(SRC_DIR)/config.ml $(OBJ_DIR)/config.cmi $(OBJ_DIR)/parseur.cmo $(OBJ_DIR)/state.cmo 
	$(CC) -c $(FLAGS) $< -I $(OBJ_DIR)/ -o $@
	
$(OBJ_DIR)/exec.cmo : $(SRC_DIR)/exec.ml $(OBJ_DIR)/parseur.cmo $(OBJ_DIR)/state.cmo $(OBJ_DIR)/config.cmo
	$(CC) -c $(FLAGS) $< -I $(OBJ_DIR)/ -o $@



clean :
	rm -r $(OBJ_DIR)/
	rm -r $(BIN_DIR)/

