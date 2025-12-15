`timescale 1ns/1ps
import uvm_pkg::*;
`include "uvm_macros.svh"
//====================================================================
// interface
//====================================================================
interface async_fifo_if #(parameter DATA_WIDTH = 8);
  logic w_clk, r_clk;
  logic w_rst_n, r_rst_n;
  logic w_en;
  logic [DATA_WIDTH-1:0] w_data;
  logic full;
  logic r_en;
  logic [DATA_WIDTH-1:0] r_data;
  logic empty;
endinterface

//====================================================================
// transaction
//====================================================================
class fifo_txn extends uvm_sequence_item;
  rand bit write;
  rand bit read;
  rand bit [7:0] data;

  constraint valid_c { write ^ read; }

  `uvm_object_utils(fifo_txn)
  function new(string name="fifo_txn");
    super.new(name);
  endfunction
endclass

//====================================================================
// random sequence
//====================================================================
class fifo_random_seq extends uvm_sequence #(fifo_txn);
  `uvm_object_utils(fifo_random_seq)
  function new(string name="fifo_random_seq");
    super.new(name);
  endfunction

  task body();
    fifo_txn tr;
    repeat (200) begin
      tr = fifo_txn::type_id::create("tr");
      assert(tr.randomize());
      start_item(tr);
      finish_item(tr);
    end
  endtask
endclass

//====================================================================
// driver
//====================================================================
class fifo_driver extends uvm_driver #(fifo_txn);
  virtual async_fifo_if vif;
  uvm_analysis_port #(fifo_txn) wr_ap;

  `uvm_component_utils(fifo_driver)

  function new(string name, uvm_component parent);
    super.new(name, parent);
    wr_ap = new("wr_ap", this);
  endfunction

  function void build_phase(uvm_phase phase);
  	if (!uvm_config_db#(virtual async_fifo_if)::get(this,"","vif",vif))
    	`uvm_fatal("DRV", "Virtual interface not set in fifo_driver")
  endfunction

  task run_phase(uvm_phase phase);
    fifo_txn tr;
    @(posedge vif.w_rst_n);
    @(posedge vif.r_rst_n);

    forever begin
      seq_item_port.get_next_item(tr);

      if (tr.write) begin
        @(posedge vif.w_clk);
        if (!vif.full) begin
          vif.w_en <= 1;
          vif.w_data <= tr.data;
          wr_ap.write(tr);
        end
        @(posedge vif.w_clk);
        vif.w_en <= 0;
      end

      if (tr.read) begin
        @(posedge vif.r_clk);
        if (!vif.empty) vif.r_en <= 1;
        @(posedge vif.r_clk);
        vif.r_en <= 0;
      end

      seq_item_port.item_done();
    end
  endtask
endclass

//====================================================================
// monitor
//====================================================================
class fifo_monitor extends uvm_monitor;
  virtual async_fifo_if vif;
  uvm_analysis_port #(fifo_txn) rd_ap;

  `uvm_component_utils(fifo_monitor)

  function new(string name, uvm_component parent);
    super.new(name,parent);
    rd_ap = new("rd_ap", this);
  endfunction
  function void build_phase(uvm_phase phase);
  	if (!uvm_config_db#(virtual async_fifo_if)::get(this,"","vif",vif))
      `uvm_fatal("MON", "Virtual interface not set in fifo_monitor")
  endfunction

  task run_phase(uvm_phase phase);
    fifo_txn tr;
    forever begin
      @(posedge vif.r_clk);
      if (vif.r_en && !vif.empty) begin
        tr = fifo_txn::type_id::create("tr");
        tr.data = vif.r_data;
        rd_ap.write(tr);
      end
    end
  endtask
endclass

//====================================================================
// scoreboard
//====================================================================
class fifo_scoreboard extends uvm_scoreboard;
  uvm_analysis_imp #(fifo_txn, fifo_scoreboard) rd_imp;
  uvm_analysis_imp #(fifo_txn, fifo_scoreboard) wr_imp;
  bit [7:0] exp_q[$];

  `uvm_component_utils(fifo_scoreboard)

  function new(string name, uvm_component parent);
    super.new(name,parent);
    rd_imp = new("rd_imp",this);
    wr_imp = new("wr_imp",this);
  endfunction

  function void write(fifo_txn tr);
    bit [7:0] exp = exp_q.pop_front();
    if (exp !== tr.data)
      `uvm_error("SB",$sformatf("EXP=%0h ACT=%0h",exp,tr.data))
  endfunction

  function void write_wr(fifo_txn tr);
    exp_q.push_back(tr.data);
  endfunction
endclass

//====================================================================
// coverage
//====================================================================
class fifo_coverage extends uvm_component;
  virtual async_fifo_if vif;

  covergroup cg;
    full_cp  : coverpoint vif.full;
    empty_cp : coverpoint vif.empty;
    sim_rw   : coverpoint (vif.w_en && vif.r_en);
  endgroup

  `uvm_component_utils(fifo_coverage)

  function new(string name, uvm_component parent);
  	super.new(name,parent);
  	cg = new();   // LEGAL PLACE
  endfunction

  function void build_phase(uvm_phase phase);
  	if (!uvm_config_db#(virtual async_fifo_if)::get(this,"","vif",vif))
      `uvm_fatal("COV", "Virtual interface not set")
  endfunction

  task run_phase(uvm_phase phase);
    forever begin
      @(posedge vif.w_clk or posedge vif.r_clk);
      cg.sample();
    end
  endtask
endclass

//====================================================================
// agent
//====================================================================
class fifo_agent extends uvm_agent;
  fifo_driver drv;
  fifo_monitor mon;
  fifo_coverage cov;
  uvm_sequencer #(fifo_txn) seqr;

  `uvm_component_utils(fifo_agent)
  function new(string name, uvm_component parent);
    super.new(name,parent);
  endfunction

  function void build_phase(uvm_phase phase);
    seqr = uvm_sequencer#(fifo_txn)::type_id::create("seqr",this);
    drv  = fifo_driver ::type_id::create("drv", this);
    mon  = fifo_monitor::type_id::create("mon", this);
    cov  = fifo_coverage::type_id::create("cov", this);
  endfunction

  function void connect_phase(uvm_phase phase);
    drv.seq_item_port.connect(seqr.seq_item_export);
  endfunction
endclass

//====================================================================
// environment
//==================================================================== 
class fifo_env extends uvm_env;
  fifo_agent agent;
  fifo_scoreboard sb;

  `uvm_component_utils(fifo_env)
  function new(string name, uvm_component parent);
    super.new(name,parent);
  endfunction

  function void build_phase(uvm_phase phase);
    agent = fifo_agent::type_id::create("agent",this);
    sb    = fifo_scoreboard::type_id::create("sb",this);
  endfunction

  function void connect_phase(uvm_phase phase);
    agent.drv.wr_ap.connect(sb.wr_imp);
    agent.mon.rd_ap.connect(sb.rd_imp);
  endfunction
endclass

//====================================================================
// test
//==================================================================== 
class fifo_test extends uvm_test;
  fifo_env env;
  `uvm_component_utils(fifo_test)

  function new(string name, uvm_component parent);
    super.new(name,parent);
  endfunction

  function void build_phase(uvm_phase phase);
    env = fifo_env::type_id::create("env",this);
  endfunction

  task run_phase(uvm_phase phase);
 	fifo_random_seq seq;

 	phase.raise_objection(this);

 	seq = fifo_random_seq::type_id::create("seq");
 	seq.start(env.agent.seqr);

 	// allow monitors/coverage to finish
 	phase.phase_done.set_drain_time(this, 1000ns);

  	phase.drop_objection(this);
  endtask
endclass

//====================================================================
// top
//====================================================================
module tb_top;
  async_fifo_if #(8) vif();

  async_fifo dut (
    .w_clk(vif.w_clk), .w_rst_n(vif.w_rst_n),
    .w_en(vif.w_en),   .w_data(vif.w_data), .full(vif.full),
    .r_clk(vif.r_clk), .r_rst_n(vif.r_rst_n),
    .r_en(vif.r_en),   .r_data(vif.r_data), .empty(vif.empty)
  );

  initial begin vif.w_clk=0; forever #5 vif.w_clk=~vif.w_clk; end
  initial begin vif.r_clk=0; forever #7 vif.r_clk=~vif.r_clk; end

  initial begin
    vif.w_rst_n=0; vif.r_rst_n=0;
    #50; vif.w_rst_n=1; vif.r_rst_n=1;
  end

  initial begin
    uvm_config_db#(virtual async_fifo_if)::set(null,"*","vif",vif);
    run_test("fifo_test");
  end
  
  initial begin
  	vif.w_en   = 0;
  	vif.r_en   = 0;
  	vif.w_data = 0;
  end
  initial begin
    #20000;
    $display("SIMULATION FINISHED");
    $finish;
  end
endmodule
