#ifndef PROCESSR_H
#define PROCESSR_H

typedef struct
{
  int x;
  int y;
  int z;
} Matrix;

void print_process_info(char *pid);
Matrix *convert_to_matrix();
char *int_to_string(int i);
void calculate_and_write_to_file(int file_desc, Matrix *matrix, int matrix_count);
int determine_which_letter(int i, Matrix matrix);

#endif