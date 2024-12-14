
[![Hits](https://hits.seeyoufarm.com/api/count/incr/badge.svg?url=https%3A%2F%2Fgithub.com%2Fmoshix%2FASMG&count_bg=%2379C83D&title_bg=%23555555&icon=lastpass.svg&icon_color=%23E7E7E7&title=hits&edge_flat=false)](https://hits.seeyoufarm.com)

The University of Waterloo Assembler Level G
============================================

ASMG is a modification to IBM's level F assembler IEUASM. Extensive use was made of the program logic manual for IEUASM. The ASMG assembler permits to build larger local dictionaries (and thus assemble larger programs) than IBM's ASMF. ASMG also supports named common, and the optional support for the IBM S/360 Model 67 RPQ instructions (ie DAT). 

ASMG excels in:

- exensively reducing the time taken to for an assembly run
- providing a batch processor for student and other small assemblies
- producing a cross reference dictionary by generating the same information as ASM F but in many fewer pages
- allowing the user to select optional instruction sets. 

How to install ASMG 
===================

The version included here has been extensively enhanced  by Dan Skomsky and patched to bring ASMG to level 27, and that includes all the known PTFs. Here is what he says:

"
All target libraries are named CBT593.ASMG27A.???.  Scan all JOB Streams for
'CBT593' and you will find all references to the target libraries.

I have applied various local fixes/changes/updates every here and there.  Scan for
my last name, 'Skomsky' to find these.

I used the UPDATE facility within Assembler G to apply all SOURCE updates except
for module ASMGF1.  Updates for ASMGF1 were applied manually.  DO NOT TRUST ThE
SEQUENCE NUMBERS WITHIN ASMGF1.  I had to create the sequence numbers manually
to apply theSE updates.  I took the easy way out for this task.  I cheated by
backing into the sequence numbers from the top of thes source deck and from the
ass end of the source deck.  The sequence numbers then converge somewhere in the
middle of the member.  It ain't pretty, but it works.

All ASMG27A updates for MACROS were manually applied.  There were only four (4)
macros updated anyway.

During the update process of Assembler G, I moved ALL MACROS within the SOURCE
and placed them into the MACLIB where they should have been all along.  Duplicate
MACROS were removed.

While testing various GENERATION options, I encountered Assembly time errors with
some MACROS and source members.  I applied local fixes to overcome these errors.
Again, scan for my last name and you will see how I did this.

I have tested this code the best way I know how.  It should work exactly as The
University of Waterloo designed it to.  But I cannot guarantee anything.

After you have configured all Job Streams for your computer, run the Job streams
in the following sequence to get everthing working:

1) MACLIB.ASMG27A.JCLA to load the MACLIB PDS.
2) LKEDCTL.ASMG27A.JCLA to load the Linkage Editor control PDS.
3) SOURCE.ASMG27A.JCLA to load the SOURCE PDS.  This is a large file and contains
   43,000 statements.
4) ASMGBASE.ASMG27A.JCLA to Assemble all modules and load the OBJLIB PDS.
5) ASMGLINK.ASMG27A.JCLA to create the LOADLIB PDS with Assembler G (ASMGASM).
"

Documentation
=============

Documentation on how to use this assembler can be found here: http://www.bitsavers.org/pdf/univOfWaterloo/V2L7_ASMG_University_of_Waterloo_Assembler_G_Usage_Guide_10th_ed_197606.pdf


Moshix, January 2025 
Whidbey Island, WA

