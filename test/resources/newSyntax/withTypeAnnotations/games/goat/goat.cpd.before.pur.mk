eval x0 = (fresh x12, x11, x10, x9, x8, x7, x5, x4, x1, x2, x3 in (((x12 == False & x11 == False & x10 == False & x9 == True & x8 == True & x7 == True & x5 == Quad (False) (False) (False) (False) & x4 == Quad (True) (True) (True) (True) & x0 == (x1 :: x2)) & step_Eval x1 x2 x3)));

step_Eval x1 x2 x3 = (fresh x22, x21, x20, x19, x18, x17, x16, x15, x14, x13, x23, x24, x25, x26, x27, x28 in ((((x22 == Quad (False) (False) (False) (True) & x21 == Quad (True) (True) (True) (False) & x20 == False & x19 == False & x18 == False & x17 == False & x16 == True & x15 == True & x14 == True & x13 == True & x3 == (Quad (x13) (x14) (x15) (False), Quad (x17) (x18) (x19) (True)) & x2 == (x23 :: x24) & x1 == Empty) & (stepEval x23 x24 x25 & _________safe_ True True True & __________safe_ False False False)) | ((x25 == Quad (True) (False) (False) (True) & x24 == Quad (False) (True) (True) (False) & x23 == True & x22 == True & x21 == True & x20 == False & x19 == False & x18 == False & x17 == False & x16 == True & x15 == True & x14 == True & x13 == True & x3 == (Quad (False) (x14) (x15) (False), Quad (True) (x18) (x19) (True)) & x2 == (x26 :: x27) & x1 == Goat) & (__stepEval x26 x27 x28 & _________safe_ False True True & __________safe_ True False False)) | ((x25 == Quad (False) (True) (False) (True) & x24 == Quad (True) (False) (True) (False) & x23 == True & x22 == True & x21 == True & x20 == False & x19 == False & x18 == False & x17 == False & x16 == True & x15 == True & x14 == True & x13 == True & x3 == (Quad (x13) (False) (x15) (False), Quad (x18) (True) (x19) (True)) & x2 == (x26 :: x27) & x1 == Wolf) & (________________stepEval x26 x27 x28 & _________safe_ True False True & __________safe_ False True False)) | ((x25 == Quad (False) (False) (True) (True) & x24 == Quad (True) (True) (False) (False) & x23 == True & x22 == True & x21 == True & x20 == False & x19 == False & x18 == False & x17 == False & x16 == True & x15 == True & x14 == True & x13 == True & x3 == (Quad (x13) (x14) (False) (False), Quad (x17) (x18) (True) (True)) & x2 == (x26 :: x27) & x1 == Cabbage) & (____________________stepEval x26 x27 x28 & _________safe_ True True False & __________safe_ False False True)))));

safe_  = (fresh x37, x36, x35, x34, x33, x32, x31, x30, x29, x28, x27, x26 in ((x37 == True & x36 == True & x35 == True & x34 == False & x33 == True & x32 == True & x31 == False & x30 == True & x29 == True & x28 == False & x27 == True & x26 == True)));

_safe_  = (fresh x28, x27, x26 in ((x28 == False & x27 == False & x26 == False)));

stepEval x23 x24 x25 = (fresh x34, x33, x32, x31, x30, x29, x28, x35, x36, x27, x26 in (((x34 == True & x33 == True & x32 == True & x31 == False & x30 == False & x29 == False & x28 == (x35, x36) & x27 == Quad (False) (False) (False) (True) & x26 == Quad (True) (True) (True) (False) & x25 == (x36, x35)) & _step_Eval x23 x24 x35 x36)));

_step_Eval x23 x24 x35 x36 = (fresh x46, x45, x44, x43, x42, x41, x40, x39, x38, x37, x47, x48, x49 in (((x46 == Quad (True) (True) (True) (True) & x45 == Quad (False) (False) (False) (False) & x44 == False & x43 == True & x42 == True & x41 == True & x40 == True & x39 == False & x38 == False & x37 == False & x36 == Quad (x41) (x42) (x43) (True) & x35 == Quad (x37) (x38) (x39) (False) & x24 == (x47 :: x48) & x23 == Empty) & (_stepEval x47 x48 x49 & _________safe_ False False False & __________safe_ True True True))));

__safe_  = (fresh x55, x54, x53, x52, x51, x50 in ((x55 == False & x54 == False & x53 == False & x52 == False & x51 == False & x50 == False)));

___safe_  = (fresh x52, x51, x50 in ((x52 == True & x51 == True & x50 == True)));

_stepEval x47 x48 x49 = (fresh x58, x57, x56, x55, x54, x53, x51, x50 in (((x58 == False & x57 == False & x56 == False & x55 == True & x54 == True & x53 == True & x51 == Quad (False) (False) (False) (False) & x50 == Quad (True) (True) (True) (True)) & step_Eval x47 x48 x49)));

____safe_  = (fresh x34, x33, x32, x31, x30, x29 in ((x34 == True & x33 == True & x32 == False & x31 == False & x30 == True & x29 == True)));

_____safe_  = (fresh x31, x30, x29 in ((x31 == False & x30 == False & x29 == True)));

__stepEval x26 x27 x28 = (fresh x37, x36, x35, x34, x33, x32, x31, x38, x39, x30, x29 in (((x37 == True & x36 == True & x35 == False & x34 == False & x33 == False & x32 == True & x31 == (x38, x39) & x30 == Quad (True) (False) (False) (True) & x29 == Quad (False) (True) (True) (False) & x28 == (x39, x38)) & __step_Eval x26 x27 x38 x39)));

__step_Eval x26 x27 x38 x39 = (fresh x49, x48, x47, x46, x45, x44, x43, x42, x41, x40, x50, x51, x52, x53, x54, x55 in ((((x49 == Quad (False) (True) (True) (True) & x48 == Quad (True) (False) (False) (False) & x47 == False & x46 == True & x45 == True & x44 == False & x43 == True & x42 == False & x41 == False & x40 == True & x39 == Quad (x44) (x45) (x46) (True) & x38 == Quad (x40) (x41) (x42) (False) & x27 == (x50 :: x51) & x26 == Empty) & (___stepEval x50 x51 x52 & _________safe_ True False False & __________safe_ False True True)) | ((x52 == Quad (True) (True) (True) (True) & x51 == Quad (False) (False) (False) (False) & x50 == True & x49 == False & x48 == False & x47 == False & x46 == True & x45 == True & x44 == False & x43 == True & x42 == False & x41 == False & x40 == True & x39 == Quad (True) (x45) (x46) (True) & x38 == Quad (False) (x41) (x42) (False) & x27 == (x53 :: x54) & x26 == Goat) & (_stepEval x53 x54 x55 & _________safe_ False False False & __________safe_ True True True)))));

______safe_  = (fresh x64, x63, x62, x61, x60, x59, x58, x57, x56, x55, x54, x53 in ((x64 == False & x63 == False & x62 == True & x61 == False & x60 == False & x59 == False & x58 == False & x57 == False & x56 == True & x55 == False & x54 == False & x53 == True)));

_______safe_  = (fresh x55, x54, x53 in ((x55 == True & x54 == True & x53 == False)));

___stepEval x50 x51 x52 = (fresh x61, x60, x59, x58, x57, x56, x54, x53 in (((x61 == False & x60 == False & x59 == True & x58 == True & x57 == True & x56 == False & x54 == Quad (True) (False) (False) (False) & x53 == Quad (False) (True) (True) (True)) & ___step_Eval x50 x51 x52)));

___step_Eval x50 x51 x52 = (fresh x71, x70, x69, x68, x67, x66, x65, x64, x63, x62, x72, x73, x74, x75, x76, x77 in ((((x71 == Quad (True) (False) (False) (True) & x70 == Quad (False) (True) (True) (False) & x69 == False & x68 == False & x67 == False & x66 == True & x65 == True & x64 == True & x63 == True & x62 == False & x52 == (Quad (x62) (x63) (x64) (False), Quad (x66) (x67) (x68) (True)) & x51 == (x72 :: x73) & x50 == Empty) & (__stepEval x72 x73 x74 & _________safe_ False True True & __________safe_ True False False)) | ((x74 == Quad (False) (True) (False) (True) & x73 == Quad (False) (False) (True) (False) & x72 == True & x71 == True & x70 == False & x69 == False & x68 == False & x67 == False & x66 == True & x65 == True & x64 == True & x63 == True & x62 == False & x52 == (Quad (x62) (False) (x64) (False), Quad (x67) (True) (x68) (True)) & x51 == (x75 :: x76) & x50 == Wolf) & (____stepEval x75 x76 x77 & _________safe_ False False True & __________safe_ False True False)) | ((x74 == Quad (True) (False) (True) (True) & x73 == Quad (False) (True) (False) (False) & x72 == True & x71 == True & x70 == False & x69 == False & x68 == False & x67 == False & x66 == True & x65 == True & x64 == True & x63 == True & x62 == False & x52 == (Quad (x62) (x63) (False) (False), Quad (x66) (x67) (True) (True)) & x51 == (x75 :: x76) & x50 == Cabbage) & (____________stepEval x75 x76 x77 & _________safe_ False True False & __________safe_ True False True)))));

________safe_  = (fresh x83, x82, x81, x80, x79, x78 in ((x83 == True & x82 == False & x81 == False & x80 == False & x79 == True & x78 == False)));

____stepEval x75 x76 x77 = (fresh x86, x85, x84, x83, x82, x81, x80, x87, x88, x79, x78 in (((x86 == True & x85 == False & x84 == False & x83 == False & x82 == True & x81 == False & x80 == (x87, x88) & x79 == Quad (False) (True) (False) (True) & x78 == Quad (False) (False) (True) (False) & x77 == (x88, x87)) & ____step_Eval x75 x76 x87 x88)));

____step_Eval x75 x76 x87 x88 = (fresh x98, x97, x96, x95, x94, x93, x92, x91, x90, x89, x99, x100, x101, x102, x103, x104 in ((((x98 == Quad (False) (False) (True) (True) & x97 == Quad (False) (True) (False) (False) & x96 == False & x95 == True & x94 == False & x93 == False & x92 == True & x91 == False & x90 == True & x89 == False & x88 == Quad (x93) (x94) (x95) (True) & x87 == Quad (x89) (x90) (x91) (False) & x76 == (x99 :: x100) & x75 == Empty) & (_____stepEval x99 x100 x101 & _________safe_ False True False & __________safe_ False False True)) | ((x101 == Quad (False) (True) (True) (True) & x100 == Quad (False) (False) (False) (False) & x99 == True & x98 == False & x97 == False & x96 == False & x95 == True & x94 == False & x93 == False & x92 == True & x91 == False & x90 == True & x89 == False & x88 == Quad (x94) (True) (x95) (True) & x87 == Quad (x89) (False) (x91) (False) & x76 == (x102 :: x103) & x75 == Wolf) & (__________stepEval x102 x103 x104 & _________safe_ False False False & __________safe_ False True True)))));

_____stepEval x99 x100 x101 = (fresh x110, x109, x108, x107, x106, x105, x103, x102 in (((x110 == False & x109 == True & x108 == False & x107 == True & x106 == False & x105 == False & x103 == Quad (False) (True) (False) (False) & x102 == Quad (False) (False) (True) (True)) & _____step_Eval x99 x100 x101)));

_____step_Eval x99 x100 x101 = (fresh x120, x119, x118, x117, x116, x115, x114, x113, x112, x111, x121, x122, x123, x124, x125, x126 in ((((x120 == Quad (False) (True) (False) (True) & x119 == Quad (False) (False) (True) (False) & x118 == False & x117 == False & x116 == True & x115 == False & x114 == True & x113 == True & x112 == False & x111 == False & x101 == (Quad (x111) (x112) (x113) (False), Quad (x115) (x116) (x117) (True)) & x100 == (x121 :: x122) & x99 == Empty) & (____stepEval x121 x122 x123 & _________safe_ False False True & __________safe_ False True False)) | ((x123 == Quad (False) (True) (True) (True) & x122 == Quad (False) (False) (False) (False) & x121 == True & x120 == False & x119 == False & x118 == False & x117 == False & x116 == True & x115 == False & x114 == True & x113 == True & x112 == False & x111 == False & x101 == (Quad (x111) (x112) (False) (False), Quad (x115) (x116) (True) (True)) & x100 == (x124 :: x125) & x99 == Cabbage) & (______stepEval x124 x125 x126 & _________safe_ False False False & __________safe_ False True True)))));

______stepEval x124 x125 x126 = (fresh x135, x134, x133, x132, x131, x130, x129, x136, x137, x128, x127 in (((x135 == False & x134 == False & x133 == False & x132 == True & x131 == True & x130 == False & x129 == (x136, x137) & x128 == Quad (False) (True) (True) (True) & x127 == Quad (False) (False) (False) (False) & x126 == (x137, x136)) & ______step_Eval x124 x125 x136 x137)));

______step_Eval x124 x125 x136 x137 = (fresh x147, x146, x145, x144, x143, x142, x141, x140, x139, x138, x148, x149, x150, x151, x152, x153 in ((((x147 == Quad (False) (False) (False) (True) & x146 == Quad (False) (True) (True) (False) & x145 == False & x144 == False & x143 == False & x142 == False & x141 == True & x140 == True & x139 == True & x138 == False & x137 == Quad (x142) (x143) (x144) (True) & x136 == Quad (x138) (x139) (x140) (False) & x125 == (x148 :: x149) & x124 == Empty) & (_______stepEval x148 x149 x150 & _________safe_ False True True & __________safe_ False False False)) | ((x150 == Quad (False) (True) (False) (True) & x149 == Quad (False) (False) (True) (False) & x148 == True & x147 == True & x146 == False & x145 == False & x144 == False & x143 == False & x142 == False & x141 == True & x140 == True & x139 == True & x138 == False & x137 == Quad (x143) (True) (x144) (True) & x136 == Quad (x138) (False) (x140) (False) & x125 == (x151 :: x152) & x124 == Wolf) & (________stepEval x151 x152 x153 & _________safe_ False False True & __________safe_ False True False)) | ((x150 == Quad (False) (False) (True) (True) & x149 == Quad (False) (True) (False) (False) & x148 == True & x147 == True & x146 == False & x145 == False & x144 == False & x143 == False & x142 == False & x141 == True & x140 == True & x139 == True & x138 == False & x137 == Quad (x142) (x143) (True) (True) & x136 == Quad (x138) (x139) (False) (False) & x125 == (x151 :: x152) & x124 == Cabbage) & (_____stepEval x151 x152 x153 & _________safe_ False True False & __________safe_ False False True)))));

_______stepEval x148 x149 x150 = (fresh x159, x158, x157, x156, x155, x154, x152, x151 in (((x159 == True & x158 == True & x157 == False & x156 == False & x155 == False & x154 == False & x152 == Quad (False) (True) (True) (False) & x151 == Quad (False) (False) (False) (True)) & _______step_Eval x148 x149 x150)));

_______step_Eval x148 x149 x150 = (fresh x169, x168, x167, x166, x165, x164, x163, x162, x161, x160, x170, x171, x172 in (((x169 == Quad (False) (True) (True) (True) & x168 == Quad (False) (False) (False) (False) & x167 == False & x166 == True & x165 == True & x164 == False & x163 == True & x162 == False & x161 == False & x160 == False & x150 == (Quad (x160) (x161) (x162) (False), Quad (x164) (x165) (x166) (True)) & x149 == (x170 :: x171) & x148 == Empty) & (______stepEval x170 x171 x172 & _________safe_ False False False & __________safe_ False True True))));

________stepEval x151 x152 x153 = (fresh x162, x161, x160, x159, x158, x157, x155, x154 in (((x162 == True & x161 == False & x160 == False & x159 == False & x158 == True & x157 == False & x155 == Quad (False) (False) (True) (False) & x154 == Quad (False) (True) (False) (True)) & ________step_Eval x151 x152 x153)));

________step_Eval x151 x152 x153 = (fresh x172, x171, x170, x169, x168, x167, x166, x165, x164, x163, x173, x174, x175, x176, x177, x178 in ((((x172 == Quad (False) (False) (True) (True) & x171 == Quad (False) (True) (False) (False) & x170 == False & x169 == True & x168 == False & x167 == False & x166 == True & x165 == False & x164 == True & x163 == False & x153 == (Quad (x163) (x164) (x165) (False), Quad (x167) (x168) (x169) (True)) & x152 == (x173 :: x174) & x151 == Empty) & (_________stepEval x173 x174 x175 & _________safe_ False True False & __________safe_ False False True)) | ((x175 == Quad (False) (True) (True) (True) & x174 == Quad (False) (False) (False) (False) & x173 == True & x172 == False & x171 == False & x170 == False & x169 == True & x168 == False & x167 == False & x166 == True & x165 == False & x164 == True & x163 == False & x153 == (Quad (x163) (False) (x165) (False), Quad (x168) (True) (x169) (True)) & x152 == (x176 :: x177) & x151 == Wolf) & (______stepEval x176 x177 x178 & _________safe_ False False False & __________safe_ False True True)))));

_________stepEval x173 x174 x175 = (fresh x184, x183, x182, x181, x180, x179, x178, x185, x186, x177, x176 in (((x184 == False & x183 == True & x182 == False & x181 == True & x180 == False & x179 == False & x178 == (x185, x186) & x177 == Quad (False) (False) (True) (True) & x176 == Quad (False) (True) (False) (False) & x175 == (x186, x185)) & (_step_ x173 ((x185, x186)) False False True False True False & _eval ((x186, x185)) x174))));

step_ x187 x188 = (fresh x200, x199, x198, x197, x196, x195, x194, x193, x192, x191, x203, x202, x201 in ((((x200 == Quad (False) (True) (False) (True) & x199 == Quad (False) (False) (True) (False) & x198 == False & x197 == False & x196 == True & x195 == False & x194 == True & x193 == True & x192 == False & x191 == False & x188 == Empty & x187 == (Quad (x191) (x192) (x193) (False), Quad (x195) (x196) (x197) (True))) & (_________safe_ False False True & __________safe_ False True False)) | ((x203 == Quad (False) (True) (True) (True) & x202 == Quad (False) (False) (False) (False) & x201 == True & x200 == False & x199 == False & x198 == False & x197 == False & x196 == True & x195 == False & x194 == True & x193 == True & x192 == False & x191 == False & x188 == Cabbage & x187 == (Quad (x191) (x192) (False) (False), Quad (x195) (x196) (True) (True))) & (_________safe_ False False False & __________safe_ False True True)))));

_eval x189 x190 = (fresh x191, x192, x193 in (((x190 == [] & x189 == (Quad (False) (False) (False) (False), Quad (True) (True) (True) (True))) | (x190 == (x191 :: x192) & (step x189 x191 x193 & _eval x193 x192)))));

step x189 x191 x193 = (fresh x195, x200, x201, x202, x194, x197, x198, x199, x196, x203, x204 in ((((x195 == Quad (x200) (x201) (x202) (False) & x194 == Quad (x197) (x198) (x199) (True) & x189 == (x194, x195)) & _step_ x191 x193 x197 x198 x199 x200 x201 x202) | ((x196 == (x203, x204) & x195 == Quad (x197) (x198) (x199) (True) & x194 == Quad (x200) (x201) (x202) (False) & x193 == (x204, x203) & x189 == (x194, x195)) & _step_ x191 ((x203, x204)) x197 x198 x199 x200 x201 x202))));

_step_ x191 x193 x197 x198 x199 x200 x201 x202 = (fresh x212, x207, x208, x209, x211, x203, x204, x205, x210, x206, x215, x214, x213 in ((((x212 == Quad (x207) (x208) (x209) (True) & x211 == Quad (x203) (x204) (x205) (False) & x210 == False & x206 == True & x202 == x209 & x201 == x208 & x200 == x207 & x199 == x205 & x198 == x204 & x197 == x203 & x193 == (Quad (x203) (x204) (x205) (False), Quad (x207) (x208) (x209) (True)) & x191 == Empty) & (_________safe_ x203 x204 x205 & __________safe_ x207 x208 x209)) | ((x215 == Quad (True) (x208) (x209) (True) & x214 == Quad (False) (x211) (x212) (False) & x213 == True & x210 == False & x206 == True & x205 == x212 & x204 == x211 & x203 == True & x202 == x209 & x201 == x208 & x200 == x207 & x199 == x205 & x198 == x204 & x197 == x203 & x193 == (Quad (False) (x204) (x205) (False), Quad (True) (x208) (x209) (True)) & x191 == Goat) & (_________safe_ False x211 x212 & __________safe_ True x208 x209)) | ((x215 == Quad (x208) (True) (x209) (True) & x214 == Quad (x211) (False) (x212) (False) & x213 == True & x210 == False & x206 == True & x205 == x212 & x204 == True & x203 == x211 & x202 == x209 & x201 == x208 & x200 == x207 & x199 == x205 & x198 == x204 & x197 == x203 & x193 == (Quad (x203) (False) (x205) (False), Quad (x208) (True) (x209) (True)) & x191 == Wolf) & (_________safe_ x211 False x212 & __________safe_ x208 True x209)) | ((x215 == Quad (x207) (x208) (True) (True) & x214 == Quad (x211) (x212) (False) (False) & x213 == True & x210 == False & x206 == True & x205 == True & x204 == x212 & x203 == x211 & x202 == x209 & x201 == x208 & x200 == x207 & x199 == x205 & x198 == x204 & x197 == x203 & x193 == (Quad (x203) (x204) (False) (False), Quad (x207) (x208) (True) (True)) & x191 == Cabbage) & (_________safe_ x211 x212 False & __________safe_ x207 x208 True)))));

_________safe_ x203 x204 x205 = (fresh x216, x215, x214, x218, x213, x217, x224, x223, x222, x221, x220, x219 in (((x216 == False & x215 == False & x214 == x218 & x213 == x217 & x205 == x214 & x204 == x213 & x203 == False) | (x224 == True & x223 == True & x222 == True & x221 == False & x220 == True & x219 == True & x218 == False & x217 == True & x216 == True & x215 == False & x214 == True & x213 == x216 & x205 == True & x204 == x214 & x203 == x213) | (x224 == False & x223 == False & x222 == True & x221 == False & x220 == False & x219 == False & x218 == False & x217 == False & x216 == True & x215 == False & x214 == False & x213 == x216 & x205 == False & x204 == x214 & x203 == x213))));

__________safe_ x207 x208 x209 = (fresh x215, x214, x213 in ((x209 == x215 & x208 == x214 & x207 == x213)));

__________stepEval x102 x103 x104 = (fresh x113, x112, x111, x110, x109, x108, x106, x105 in (((x113 == False & x112 == False & x111 == False & x110 == True & x109 == True & x108 == False & x106 == Quad (False) (False) (False) (False) & x105 == Quad (False) (True) (True) (True)) & _________step_Eval x102 x103 x104)));

_________step_Eval x102 x103 x104 = (fresh x123, x122, x121, x120, x119, x118, x117, x116, x115, x114, x124, x125, x126, x127, x128, x129 in ((((x123 == Quad (False) (False) (False) (True) & x122 == Quad (False) (True) (True) (False) & x121 == False & x120 == False & x119 == False & x118 == False & x117 == True & x116 == True & x115 == True & x114 == False & x104 == (Quad (x114) (x115) (x116) (False), Quad (x118) (x119) (x120) (True)) & x103 == (x124 :: x125) & x102 == Empty) & (___________stepEval x124 x125 x126 & _________safe_ False True True & __________safe_ False False False)) | ((x126 == Quad (False) (True) (False) (True) & x125 == Quad (False) (False) (True) (False) & x124 == True & x123 == True & x122 == False & x121 == False & x120 == False & x119 == False & x118 == False & x117 == True & x116 == True & x115 == True & x114 == False & x104 == (Quad (x114) (False) (x116) (False), Quad (x119) (True) (x120) (True)) & x103 == (x127 :: x128) & x102 == Wolf) & (____stepEval x127 x128 x129 & _________safe_ False False True & __________safe_ False True False)) | ((x126 == Quad (False) (False) (True) (True) & x125 == Quad (False) (True) (False) (False) & x124 == True & x123 == True & x122 == False & x121 == False & x120 == False & x119 == False & x118 == False & x117 == True & x116 == True & x115 == True & x114 == False & x104 == (Quad (x114) (x115) (False) (False), Quad (x118) (x119) (True) (True)) & x103 == (x127 :: x128) & x102 == Cabbage) & (_________stepEval x127 x128 x129 & _________safe_ False True False & __________safe_ False False True)))));

___________stepEval x124 x125 x126 = (fresh x135, x134, x133, x132, x131, x130, x129, x136, x137, x128, x127 in (((x135 == True & x134 == True & x133 == False & x132 == False & x131 == False & x130 == False & x129 == (x136, x137) & x128 == Quad (False) (False) (False) (True) & x127 == Quad (False) (True) (True) (False) & x126 == (x137, x136)) & __________step_Eval x124 x125 x136 x137)));

__________step_Eval x124 x125 x136 x137 = (fresh x147, x146, x145, x144, x143, x142, x141, x140, x139, x138, x148, x149, x150 in (((x147 == Quad (False) (True) (True) (True) & x146 == Quad (False) (False) (False) (False) & x145 == False & x144 == True & x143 == True & x142 == False & x141 == True & x140 == False & x139 == False & x138 == False & x137 == Quad (x142) (x143) (x144) (True) & x136 == Quad (x138) (x139) (x140) (False) & x125 == (x148 :: x149) & x124 == Empty) & (__________stepEval x148 x149 x150 & _________safe_ False False False & __________safe_ False True True))));

____________stepEval x75 x76 x77 = (fresh x86, x85, x84, x83, x82, x81, x80, x87, x88, x79, x78 in (((x86 == False & x85 == True & x84 == False & x83 == True & x82 == False & x81 == True & x80 == (x87, x88) & x79 == Quad (True) (False) (True) (True) & x78 == Quad (False) (True) (False) (False) & x77 == (x88, x87)) & ___________step_Eval x75 x76 x87 x88)));

___________step_Eval x75 x76 x87 x88 = (fresh x98, x97, x96, x95, x94, x93, x92, x91, x90, x89, x99, x100, x101, x102, x103, x104 in ((((x98 == Quad (False) (True) (False) (True) & x97 == Quad (True) (False) (True) (False) & x96 == False & x95 == False & x94 == True & x93 == False & x92 == True & x91 == True & x90 == False & x89 == True & x88 == Quad (x93) (x94) (x95) (True) & x87 == Quad (x89) (x90) (x91) (False) & x76 == (x99 :: x100) & x75 == Empty) & (_____________stepEval x99 x100 x101 & _________safe_ True False True & __________safe_ False True False)) | ((x101 == Quad (True) (True) (False) (True) & x100 == Quad (False) (False) (True) (False) & x99 == True & x98 == True & x97 == False & x96 == False & x95 == False & x94 == True & x93 == False & x92 == True & x91 == True & x90 == False & x89 == True & x88 == Quad (True) (x94) (x95) (True) & x87 == Quad (False) (x90) (x91) (False) & x76 == (x102 :: x103) & x75 == Goat) & (______________stepEval x102 x103 x104 & _________safe_ False False True & __________safe_ True True False)) | ((x101 == Quad (False) (True) (True) (True) & x100 == Quad (True) (False) (False) (False) & x99 == True & x98 == False & x97 == True & x96 == False & x95 == False & x94 == True & x93 == False & x92 == True & x91 == True & x90 == False & x89 == True & x88 == Quad (x93) (x94) (True) (True) & x87 == Quad (x89) (x90) (False) (False) & x76 == (x102 :: x103) & x75 == Cabbage) & (___stepEval x102 x103 x104 & _________safe_ True False False & __________safe_ False True True)))));

_____________stepEval x99 x100 x101 = (fresh x110, x109, x108, x107, x106, x105, x103, x102 in (((x110 == True & x109 == False & x108 == True & x107 == False & x106 == True & x105 == False & x103 == Quad (True) (False) (True) (False) & x102 == Quad (False) (True) (False) (True)) & ____________step_Eval x99 x100 x101)));

____________step_Eval x99 x100 x101 = (fresh x120, x119, x118, x117, x116, x115, x114, x113, x112, x111, x121, x122, x123, x124, x125, x126 in ((((x120 == Quad (True) (False) (True) (True) & x119 == Quad (False) (True) (False) (False) & x118 == False & x117 == True & x116 == False & x115 == True & x114 == True & x113 == False & x112 == True & x111 == False & x101 == (Quad (x111) (x112) (x113) (False), Quad (x115) (x116) (x117) (True)) & x100 == (x121 :: x122) & x99 == Empty) & (____________stepEval x121 x122 x123 & _________safe_ False True False & __________safe_ True False True)) | ((x123 == Quad (False) (True) (True) (True) & x122 == Quad (False) (False) (False) (False) & x121 == True & x120 == False & x119 == False & x118 == False & x117 == True & x116 == False & x115 == True & x114 == True & x113 == False & x112 == True & x111 == False & x101 == (Quad (x111) (False) (x113) (False), Quad (x116) (True) (x117) (True)) & x100 == (x124 :: x125) & x99 == Wolf) & (______stepEval x124 x125 x126 & _________safe_ False False False & __________safe_ False True True)))));

______________stepEval x102 x103 x104 = (fresh x113, x112, x111, x110, x109, x108, x106, x105 in (((x113 == True & x112 == False & x111 == False & x110 == False & x109 == True & x108 == True & x106 == Quad (False) (False) (True) (False) & x105 == Quad (True) (True) (False) (True)) & _____________step_Eval x102 x103 x104)));

_____________step_Eval x102 x103 x104 = (fresh x123, x122, x121, x120, x119, x118, x117, x116, x115, x114, x124, x125, x126, x127, x128, x129 in ((((x123 == Quad (False) (False) (True) (True) & x122 == Quad (True) (True) (False) (False) & x121 == False & x120 == True & x119 == False & x118 == False & x117 == True & x116 == False & x115 == True & x114 == True & x104 == (Quad (x114) (x115) (x116) (False), Quad (x118) (x119) (x120) (True)) & x103 == (x124 :: x125) & x102 == Empty) & (____________________stepEval x124 x125 x126 & _________safe_ True True False & __________safe_ False False True)) | ((x126 == Quad (True) (False) (True) (True) & x125 == Quad (False) (True) (False) (False) & x124 == True & x123 == False & x122 == True & x121 == False & x120 == True & x119 == False & x118 == False & x117 == True & x116 == False & x115 == True & x114 == True & x104 == (Quad (False) (x115) (x116) (False), Quad (True) (x119) (x120) (True)) & x103 == (x127 :: x128) & x102 == Goat) & (____________stepEval x127 x128 x129 & _________safe_ False True False & __________safe_ True False True)) | ((x126 == Quad (False) (True) (True) (True) & x125 == Quad (True) (False) (False) (False) & x124 == True & x123 == False & x122 == True & x121 == False & x120 == True & x119 == False & x118 == False & x117 == True & x116 == False & x115 == True & x114 == True & x104 == (Quad (x114) (False) (x116) (False), Quad (x119) (True) (x120) (True)) & x103 == (x127 :: x128) & x102 == Wolf) & (_______________stepEval x127 x128 x129 & _________safe_ True False False & __________safe_ False True True)))));

_______________stepEval x127 x128 x129 = (fresh x138, x137, x136, x135, x134, x133, x132, x139, x140, x131, x130 in (((x138 == False & x137 == False & x136 == True & x135 == True & x134 == True & x133 == False & x132 == (x139, x140) & x131 == Quad (False) (True) (True) (True) & x130 == Quad (True) (False) (False) (False) & x129 == (x140, x139)) & (_step_ x127 ((x139, x140)) False True True True False False & _eval ((x140, x139)) x128))));

________________stepEval x26 x27 x28 = (fresh x37, x36, x35, x34, x33, x32, x31, x38, x39, x30, x29 in (((x37 == True & x36 == False & x35 == True & x34 == False & x33 == True & x32 == False & x31 == (x38, x39) & x30 == Quad (False) (True) (False) (True) & x29 == Quad (True) (False) (True) (False) & x28 == (x39, x38)) & ______________step_Eval x26 x27 x38 x39)));

______________step_Eval x26 x27 x38 x39 = (fresh x49, x48, x47, x46, x45, x44, x43, x42, x41, x40, x50, x51, x52, x53, x54, x55 in ((((x49 == Quad (True) (False) (True) (True) & x48 == Quad (False) (True) (False) (False) & x47 == False & x46 == True & x45 == False & x44 == True & x43 == True & x42 == False & x41 == True & x40 == False & x39 == Quad (x44) (x45) (x46) (True) & x38 == Quad (x40) (x41) (x42) (False) & x27 == (x50 :: x51) & x26 == Empty) & (_________________stepEval x50 x51 x52 & _________safe_ False True False & __________safe_ True False True)) | ((x52 == Quad (False) (True) (True) (True) & x51 == Quad (False) (False) (False) (False) & x50 == True & x49 == False & x48 == False & x47 == False & x46 == True & x45 == False & x44 == True & x43 == True & x42 == False & x41 == True & x40 == False & x39 == Quad (x45) (True) (x46) (True) & x38 == Quad (x40) (False) (x42) (False) & x27 == (x53 :: x54) & x26 == Wolf) & (__________stepEval x53 x54 x55 & _________safe_ False False False & __________safe_ False True True)))));

_________________stepEval x50 x51 x52 = (fresh x61, x60, x59, x58, x57, x56, x54, x53 in (((x61 == False & x60 == True & x59 == False & x58 == True & x57 == False & x56 == True & x54 == Quad (False) (True) (False) (False) & x53 == Quad (True) (False) (True) (True)) & _______________step_Eval x50 x51 x52)));

_______________step_Eval x50 x51 x52 = (fresh x71, x70, x69, x68, x67, x66, x65, x64, x63, x62, x72, x73, x74, x75, x76, x77 in ((((x71 == Quad (False) (True) (False) (True) & x70 == Quad (True) (False) (True) (False) & x69 == False & x68 == False & x67 == True & x66 == False & x65 == True & x64 == True & x63 == False & x62 == True & x52 == (Quad (x62) (x63) (x64) (False), Quad (x66) (x67) (x68) (True)) & x51 == (x72 :: x73) & x50 == Empty) & (________________stepEval x72 x73 x74 & _________safe_ True False True & __________safe_ False True False)) | ((x74 == Quad (True) (True) (False) (True) & x73 == Quad (False) (False) (True) (False) & x72 == True & x71 == True & x70 == False & x69 == False & x68 == False & x67 == True & x66 == False & x65 == True & x64 == True & x63 == False & x62 == True & x52 == (Quad (False) (x63) (x64) (False), Quad (True) (x67) (x68) (True)) & x51 == (x75 :: x76) & x50 == Goat) & (__________________stepEval x75 x76 x77 & _________safe_ False False True & __________safe_ True True False)) | ((x74 == Quad (False) (True) (True) (True) & x73 == Quad (True) (False) (False) (False) & x72 == True & x71 == False & x70 == True & x69 == False & x68 == False & x67 == True & x66 == False & x65 == True & x64 == True & x63 == False & x62 == True & x52 == (Quad (x62) (x63) (False) (False), Quad (x66) (x67) (True) (True)) & x51 == (x75 :: x76) & x50 == Cabbage) & (_______________stepEval x75 x76 x77 & _________safe_ True False False & __________safe_ False True True)))));

__________________stepEval x75 x76 x77 = (fresh x86, x85, x84, x83, x82, x81, x80, x87, x88, x79, x78 in (((x86 == True & x85 == False & x84 == False & x83 == False & x82 == True & x81 == True & x80 == (x87, x88) & x79 == Quad (True) (True) (False) (True) & x78 == Quad (False) (False) (True) (False) & x77 == (x88, x87)) & ________________step_Eval x75 x76 x87 x88)));

________________step_Eval x75 x76 x87 x88 = (fresh x98, x97, x96, x95, x94, x93, x92, x91, x90, x89, x99, x100, x101, x102, x103, x104 in ((((x98 == Quad (False) (False) (True) (True) & x97 == Quad (True) (True) (False) (False) & x96 == False & x95 == True & x94 == False & x93 == False & x92 == True & x91 == False & x90 == True & x89 == True & x88 == Quad (x93) (x94) (x95) (True) & x87 == Quad (x89) (x90) (x91) (False) & x76 == (x99 :: x100) & x75 == Empty) & (___________________stepEval x99 x100 x101 & _________safe_ True True False & __________safe_ False False True)) | ((x101 == Quad (True) (False) (True) (True) & x100 == Quad (False) (True) (False) (False) & x99 == True & x98 == False & x97 == True & x96 == False & x95 == True & x94 == False & x93 == False & x92 == True & x91 == False & x90 == True & x89 == True & x88 == Quad (True) (x94) (x95) (True) & x87 == Quad (False) (x90) (x91) (False) & x76 == (x102 :: x103) & x75 == Goat) & (_________________stepEval x102 x103 x104 & _________safe_ False True False & __________safe_ True False True)) | ((x101 == Quad (False) (True) (True) (True) & x100 == Quad (True) (False) (False) (False) & x99 == True & x98 == False & x97 == True & x96 == False & x95 == True & x94 == False & x93 == False & x92 == True & x91 == False & x90 == True & x89 == True & x88 == Quad (x94) (True) (x95) (True) & x87 == Quad (x89) (False) (x91) (False) & x76 == (x102 :: x103) & x75 == Wolf) & (___stepEval x102 x103 x104 & _________safe_ True False False & __________safe_ False True True)))));

___________________stepEval x99 x100 x101 = (fresh x110, x109, x108, x107, x106, x105, x103, x102 in (((x110 == False & x109 == True & x108 == True & x107 == True & x106 == False & x105 == False & x103 == Quad (True) (True) (False) (False) & x102 == Quad (False) (False) (True) (True)) & _________________step_Eval x99 x100 x101)));

_________________step_Eval x99 x100 x101 = (fresh x120, x119, x118, x117, x116, x115, x114, x113, x112, x111, x121, x122, x123 in ((((x120 == Quad (True) (True) (False) (True) & x119 == Quad (False) (False) (True) (False) & x118 == False & x117 == False & x116 == True & x115 == True & x114 == True & x113 == True & x112 == False & x111 == False & x101 == (Quad (x111) (x112) (x113) (False), Quad (x115) (x116) (x117) (True)) & x100 == (x121 :: x122) & x99 == Empty) & (__________________stepEval x121 x122 x123 & _________safe_ False False True & __________safe_ True True False)) | ((x123 == Quad (True) (True) (True) (True) & x122 == Quad (False) (False) (False) (False) & x121 == True & x120 == False & x119 == False & x118 == False & x117 == False & x116 == True & x115 == True & x114 == True & x113 == True & x112 == False & x111 == False & x101 == (Quad (x111) (x112) (False) (False), Quad (x115) (x116) (True) (True)) & x99 == Cabbage) & (_________safe_ False False False & __________safe_ True True True & _eval ((Quad (False) (False) (False) (False), Quad (True) (True) (True) (True))) x100)))));

____________________stepEval x26 x27 x28 = (fresh x37, x36, x35, x34, x33, x32, x31, x38, x39, x30, x29 in (((x37 == False & x36 == True & x35 == True & x34 == True & x33 == False & x32 == False & x31 == (x38, x39) & x30 == Quad (False) (False) (True) (True) & x29 == Quad (True) (True) (False) (False) & x28 == (x39, x38)) & __________________step_Eval x26 x27 x38 x39)));

__________________step_Eval x26 x27 x38 x39 = (fresh x49, x48, x47, x46, x45, x44, x43, x42, x41, x40, x50, x51, x52, x53, x54, x55 in ((((x49 == Quad (True) (True) (False) (True) & x48 == Quad (False) (False) (True) (False) & x47 == False & x46 == False & x45 == True & x44 == True & x43 == True & x42 == True & x41 == False & x40 == False & x39 == Quad (x44) (x45) (x46) (True) & x38 == Quad (x40) (x41) (x42) (False) & x27 == (x50 :: x51) & x26 == Empty) & (______________stepEval x50 x51 x52 & _________safe_ False False True & __________safe_ True True False)) | ((x52 == Quad (True) (True) (True) (True) & x51 == Quad (False) (False) (False) (False) & x50 == True & x49 == False & x48 == False & x47 == False & x46 == False & x45 == True & x44 == True & x43 == True & x42 == True & x41 == False & x40 == False & x39 == Quad (x44) (x45) (True) (True) & x38 == Quad (x40) (x41) (False) (False) & x27 == (x53 :: x54) & x26 == Cabbage) & (_stepEval x53 x54 x55 & _________safe_ False False False & __________safe_ True True True)))));


? eval x0