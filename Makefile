CC=gcc
CFLAGS=-O0
OBJ = lambda-eval.o

%.o: %.c $(DEPS)
	$(CC) $(CFLAGS) -c -o $@ $<

lambda-eval: $(OBJ)
	$(CC) $(CFLAGS) -o $@ $^ 
