`define DELAY 20
module alu32bit_testbench();
reg [31:0] a, b;
reg s0, s1, s2;
wire [31:0] res;

// ALUT stands for alu testbench
alu32bit ALUT(res, a, b, s0, s1, s2);

initial begin
a = 32'h0; b = 32'h0; s0 = 0; s1 = 0; s2 = 0;
#`DELAY;
a = 32'h1; b = 32'h1; s0 = 1; s1 = 0; s2 = 0;
#`DELAY;
a = 32'h2; b = 32'h2; s0 = 0; s1 = 1; s2 = 0;
#`DELAY;
a = 32'h3; b = 32'h3; s0 = 1; s1 = 1; s2 = 0;
#`DELAY;
a = 32'h4; b = 32'h4; s0 = 0; s1 = 0; s2 = 1;
#`DELAY;
a = 32'h5; b = 32'h5; s0 = 1; s1 = 0; s2 = 1;
#`DELAY;
a = 32'h6; b = 32'h6; s0 = 0; s1 = 1; s2 = 1;
#`DELAY;
a = 32'h7; b = 32'h7; s0 = 1; s1 = 1; s2 = 1;
#`DELAY;
a = 32'h8; b = 32'h8; s0 = 0; s1 = 0; s2 = 0;
#`DELAY;
a = 32'h9; b = 32'h9; s0 = 1; s1 = 0; s2 = 0;
#`DELAY;
a = 32'ha; b = 32'ha; s0 = 0; s1 = 1; s2 = 0;
#`DELAY;
a = 32'hb; b = 32'hb; s0 = 1; s1 = 1; s2 = 0;
end

initial
begin
$monitor("ALU RESULTS: time = %2d, res = %8h, a =%8h, b= %8h, s0= %1b, s1= %1b, s2= %1b", $time,res, a, b, s0, s1, s2);
end
 

endmodule