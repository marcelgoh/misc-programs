CC=gcc

SRCDIR=src
INCDIR=include
OBJDIR=obj

SRCS=${wildcard ${SRCDIR}/*.c}
INCS=${wildcard ${INCDIR}/*.h}
OBJS=${patsubst ${SRCDIR}/%.c,${OBJDIR}/%.o,${SRCS}}

EXEC=main
TRASH=${OBJDIR} ${EXEC}

CFLAGS=-I${INCDIR}

$(shell mkdir -p ${OBJDIR})

${EXEC}: ${OBJS}
	${CC} ${CFLAGS} ${SRCS} -o $@

${OBJDIR}/%.o: src/%.c ${HEADERS}
	${CC} ${CFLAGS} -g -c -o $@ $<

.PHONY: clean
clean:
	rm -rf ${TRASH}
