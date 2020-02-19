#packages
library(tidyverse)
library(dplyr)
library(magrittr)
library(table1)

#PPT with TBI
TBI <- read.csv('~/OneDrive - SickKids/ABCD_TBI/ABCD_Data/TBI_FORQUALANALYSIS.csv')
colnames(TBI)[2] <- ("subjectkey")
colnames(TBI)[3] <- ("Group")
TBI <- TBI[,-1]
levels(TBI$Group) <- sub("yes", "TBI", levels(TBI$Group))

#medsy01.txt - Medications Taken in the Last Two Weeks
medrx <- read.csv('~/OneDrive - SickKids/ABCD_TBI/ABCD_Data/MEDRx_parent/medsy01.txt', sep='\t', header=T)
#data dictionary
dataDic_medrx <- medrx[1,]
dataDic_medrx <- t(dataDic_medrx)
medrx <- medrx[-1,]
medrx <- subset(medrx, eventname=="baseline_year_1_arm_1")

medrx_ALL <- merge(medrx, TBI, by='subjectkey', all.x = T)
levels <- levels(medrx_ALL$Group)
levels[length(levels) + 1] <- "CTRL"
medrx_ALL$Group <- factor(medrx_ALL$Group, levels = levels)
medrx_ALL$Group[is.na(medrx_ALL$Group)] <- "CTRL"
medrx_names <- medrx_ALL[,c(1,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,299)]

names <- as.vector(as.matrix(medrx_names[,2:16]))

unique(sort(names))  #### 974

unique(medrx_names$med1_rxnorm_p) #### 694 

# #only TBI
# medrx_names <- medrx_names[medrx_names$Group=="TBI",]

#remove numbers and other words from medication names
#names(medrx_names)
medrx_names[,c(2:16)] <- data.frame(lapply(medrx_names[c(2:16)], 
                                     function(x) 
                                       (gsub("[0-9]|\\<mg\\>|\\<MG\\>|\\<Oral\\>|\\<Pill\\>|\\<Inhalant Product\\>|Tablet\\>|\\<Product\\>|\\<Dental Product\\>|\\<Nasal\\>|\\<Ointment\\>|\\<Spray\\>|\\<Solution\\>|\\<Liquid\\>|\\<Chewable\\>|\\<ML\\>|/|\\[.*\\]|[.]|\\<Capsule\\>|\\<Delayed Release\\>||\\<Extended Release\\>|\\<Injectable\\>|\\<Topical\\>|\\<ACTUAT\\>|\\<Suspension\\>|\\<Disintegrating\\>|\\<Gel\\>|\\<Cream\\>|","", x))))

#remove white space

# str(medrx_names) #checking type
#removes leading and trailing white space
medrx_names <- data.frame(lapply(medrx_names, trimws), stringsAsFactors = FALSE)

#make all lower case
medrx_names[c(2:16)] <- mutate_all(medrx_names[c(2:16)], funs(tolower))

unique(sort(medrx_names$med1_rxnorm_p)) #### 367
# unique(sort(medrx_names[c(2:16)]))

## COLUMN 1 ##

unique(medrx_names$med1_rxnorm_p) #### 590
#fix inconsistent annotation
medrx_names <- medrx_names %>% mutate(med1_rxnorm_p = recode(med1_rxnorm_p,
                                                   `albuterol    cromolyn   inhalant` = 'albuterol',
                                                   `albuterol   inhalant` = 'albuterol',
                                                   `albuterol   metered dose inhaler` = 'albuterol', 
                                                   `albuterol  ambroxol` = 'albuterol',
                                                   `albuterol  beclomethasone` = 'albuterol',
                                                   `albuterol  ipratropium inhalant` = 'albuterol',
                                                   `albuterol inhalant powder` = 'albuterol',
                                                   `albuterol metered dose inhaler` = 'albuterol',                  
                                                   `albuterol sulfate` = 'albuterol',
                                                   `alclometasone dipropionate` = 'alclometasone',
                                                   `aller-tec d` = 'aller-tec',
                                                   `amoxi-tabs` = 'amoxicillin',
                                                   `amoxicillin    clavulanate` = 'amoxicillin',
                                                   `amoxicillin  clavulanate` = 'amoxicillin',
                                                   `amoxicillin  clonixin` = 'amoxicillin',
                                                   `amoxicillin trihydrate` = 'amoxicillin',
                                                   `amphetamine  dextroamphetamine` = 'amphetamine',
                                                   `amphetamine sulfate` = 'amphetamine',
                                                   `atomoxetine hydrochloride` = 'atomoxetine',
                                                   `atropine sulfate    hyoscyamine sulfate    phenobarbital    scopolamine hydrobromide` = 'atropine sulfate',
                                                   `azelastine ophthalmic` = "azelastine",
                                                   `bacitracin  unt  neomycin    polymyxin b  unt` = 'neomycin-bacitracnzn-polymyxnb',
                                                   `beclomethasone dipropionate` = 'beclomethasone',
                                                   `beclomethasone dipropionate   metered dose inhaler` = 'beclomethasone',
                                                   `benadryl-d allergy and sinus` = 'benadryl',
                                                   `cannabinol  tetrahydrocannabinol` = 'cannabinol',
                                                   `cetirizine hydrochloride` = 'cetirizine',
                                                   `chlorpheniramine maleate    dextromethorphan hydrobromide` = 'dextromethorphan',
                                                   `cholecalciferol  unt` = 'cholecalciferol',
                                                   `ciprodex otic` = 'ciprodex',
                                                   `claritin-d` = 'claritin',
                                                   `clonidine  hr` = 'clonidine',
                                                   `clonidine hydrochloride` = 'clonidine',
                                                   `cortizone-` = 'cortizone',
                                                   `cyproheptadine hydrochloride` = 'cyproheptadine',
                                                   `dayquil cough` = 'dayquil',
                                                   `delsym cough relief plus soothing action` = 'delsym',
                                                   `desmopressin acetate` = 'desmopressin',
                                                   `dexmethylphenidate hydrochloride` = 'dexmethylphenidate',
                                                   `dextroamphetamine saccharate` = 'dextroamphetamine',
                                                   `dextroamphetamine sulfate` = 'dextroamphetamine',
                                                   `dextromethorphan hydrobromide    guaifenesin` = 'dextromethorphan',
                                                   `dextromethorphan polistirex` = 'dextromethorphan',
                                                   `doxycycline hyclate` = 'doxycycline',
                                                   `erythromycin ophthalmic` = 'erythromycin',
                                                   `fexofenadine hydrochloride` = 'fexofenadine',
                                                   `fluticasone  vilanterol dry powder inhaler` = 'fluticasone',
                                                   `fluticasone propionate    salmeterol   dry powder inhaler` = 'fluticasone',
                                                   `fluticasone propionate    salmeterol   metered dose inhaler` = 'fluticasone',
                                                   `fluticasone propionate   metered dose inhaler` = 'fluticasone',
                                                   `fluticasone propionate   metered dose` = 'fluticasone',
                                                   `fluticasone propionate` = 'fluticasone',
                                                   `fluvoxamine maleate` = 'fluvoxamine',
                                                   `formoterol fumarate    mometasone furoate` = 'mometasone',
                                                   `gentamicin sulfate (usp) otic` = 'gentamicin sulfate',
                                                   `guaifenesin  noscapine` = 'guaifenesin',
                                                   `guaifenesin  phenylephrine` = 'guaifenesin',
                                                   `guanfacine hydrochloride` = 'guanfacine',
                                                   `hr amphetamine aspartate    amphetamine sulfate    dextroamphetamine saccharate    dextroamphetamine sulfate` = 'amphetamine',
                                                   `hr clonidine hydrochloride` = 'clonidine',
                                                   `hr dexmethylphenidate hydrochloride` = 'dexmethylphenidate',
                                                   `hr loratadine    pseudoephedrine sulfate` = 'loratadine',
                                                   `hr methylphenidate hydrochloride` = 'methylphenidate',
                                                   `hydrocodone bitartrate` = 'hydrocodone',
                                                   `hydrocortisone    lotion` = 'hydrocortisone',
                                                   `hydrocortisone    neomycin    polymyxin b  unt otic` = 'neomycin-bacitracnzn-polymyxnb',
                                                   `hydrocortisone acetate` = 'hydrocortisone',
                                                   `hydrocortisone valerate` = 'hydrocortisone',
                                                   `hydroxyzine hydrochloride` = 'hydroxyzine',
                                                   `ibuprofen    phenylephrine hydrochloride` = 'ibuprofen',
                                                   `ibuprofen    pseudoephedrine hydrochloride` = 'ibuprofen',
                                                   `imipramine hydrochloride` = 'imipramine',
                                                   `insulin glargine  unt pen injector` = 'insulin',
                                                   `insulin human, isophane  unt` = 'insulin',
                                                   `insulin lispro  insulin, protamine lispro, human pen injector` = 'insulin',
                                                   `ketotifen   ophthalmic` = 'ketotifen',
                                                   `levocetirizine dihydrochloride` = 'levocetirizine',
                                                   `levothyroxine sodium    liothyronine sodium` = 'levothyroxine',
                                                   `levothyroxine sodium` = 'levothyroxine',
                                                   `lisdexamfetamine dimesylate` = 'lisdexamfetamine',
                                                   `lotemax ophthalmic` = 'lotemax',
                                                   `melatonin  theanine` = 'melatonin',
                                                   `methylphenidate hydrochloride` = 'methylphenidate',
                                                   `miralax  powder` = 'miralax',
                                                   `mometasone  lotion` = 'mometasone',
                                                   `mometasone furoate` = 'mometasone',
                                                   `mometasone-formoterol` = 'mometasone',
                                                   `montelukast    granules` = 'montelukast',
                                                   `montelukast sodium` = 'montelukast',
                                                   `neomycin  polymyxin b` = 'neomycin-bacitracnzn-polymyxnb',
                                                   `olopatadine hydrochloride` = 'olopatadine',
                                                   `oxybutynin chloride` = 'oxybutynin',
                                                   `penicillin v potassium` = 'penicillin',
                                                   `penicillin v` = 'penicillin',
                                                   `polymyxin b  unt  trimethoprim` = 'polymyxin b',
                                                   `polyethylenes` = 'polyethylene glycol',
                                                   `sertraline hydrochloride` = 'sertraline',
                                                   `somatropin   pen injector` = 'somatropin',
                                                   `t vitamin` = 'vitamin t',
                                                   `theraflu vapor stick` = 'theraflu',
                                                   `triamcinolone  urea` = 'triamcinolone',
                                                   `triamcinolone acetonide` = 'triamcinolone',
                                                   `tylenol chest congestion` = 'tylenol',
                                                   `tylenol pm` = 'tylenol',
                                                   `tylenol severe allergy` = 'tylenol',
                                                   `tylenol simply cough` = 'tylenol',
                                                   `wal-dryl-d allergy` = 'wal-dryl-d',
                                                   `zyrtec-d` = 'zyrtec',
))

unique(sort(medrx_names$med1_rxnorm_p)) #### 275
#brand to generic 
medrx_names <- medrx_names %>% mutate(med1_rxnorm_p = recode(med1_rxnorm_p,
                                                    `abilify` = 'aripiprazole',
                                                    `actemra` = 'tocilizumab',
                                                    `adderall` = 'dextroamphetamine',
                                                    `advair` = 'fluticasone',
                                                    `advate` = 'antihemophilic factor',
                                                    `advil` = 'ibuprofen',
                                                    `adzenys` = 'amphetamine',
                                                    `aerospan` = 'flunisolide',
                                                    `alaway` = 'ketotifen',
                                                    `albenza` = 'albendazole',
                                                    `allegra` = 'fexofenadine',
                                                    `aller-tec d` = 'cetirizine',
                                                    `aller-tec` = 'cetirizine',
                                                    `alvesco` = 'ciclesonide',
                                                    `amitiza` = 'lubiprostone',
                                                    `aptensio` = 'methylphenidate',
                                                    `armonair` = 'fluticasone',
                                                    `arnuity` = 'fluticasone',
                                                    `asmanex` = 'mometasone',
                                                    `augmentin` = 'amoxicillin/clavulanate',
                                                    `auvi-q` = 'epinephrine',
                                                    `bactrim` = 'sulfamethoxazole',
                                                    `benadryl` = 'diphenhydramine',
                                                    `breo` = 'fluticasone',
                                                    `bromfed dm` = 'brompheniramine',
                                                    `buspar` = 'buspirone',
                                                    `cellcept` = 'mycophenolate mofetil',
                                                    `cipro` = 'ciprofloxacin',
                                                    `ciprodex` = 'ciprofloxacin/dexamethasone',
                                                    `claritin` = 'loratadine',
                                                    `colcrys` = 'colchicine',
                                                    `concerta`= 'methylphenidate',
                                                    `cortizone` = 'hydrocortisone',
                                                    `creon` = 'pancrelipase',
                                                    `dayquil` = 'acetaminophen',
                                                    `daytrana` = 'methylphenidate',
                                                    `delsym` = 'dextromethorphan',
                                                    `derma-smoothe fs` = 'fluocinolone acetonide',
                                                    `dulera` = 'mometasone',
                                                    `dyanavel` = 'amphetamine',
                                                    `dymista` = 'azelastine-fluticasone',
                                                    `eloctate` = 'antihemophilic factor',
                                                    `enbrel` = 'etanercept',
                                                    `epiduo` = 'adapalene-benzoyl peroxide',
                                                    `epipen` = 'epinephrine',
                                                    `evekeo` = 'amphetamine',
                                                    `flonase` = 'fluticasone',
                                                    `flovent` = 'fluticasone',
                                                    `fluoridex` = 'sodium fluoride', 
                                                    `focalin` = 'dexmethylphenidate',
                                                    `golytely` = 'polyethylene glycol',
                                                    `humalog` = 'insulin',
                                                    `humatrope` = 'somatropin',
                                                    `humira` = 'adalimumab',
                                                    `imitrex` = 'sumatriptan',
                                                    `intuniv` = 'guanfacine',
                                                    `jadenu` = 'deferasirox',
                                                    `keppra` = 'levetiracetam',
                                                    `lactaid` = 'lactase',
                                                    `lantus` = 'insulin',
                                                    `latuda` = 'lurasidone',
                                                    `levoxyl` = 'levothyroxine',
                                                    `lexapro` = 'escitalopram',
                                                    `lotemax` = 'loteprednol etabonate',
                                                    `ludent` = 'sodium fluoride',
                                                    `lupron` = 'leuprolide',
                                                    `maalox` = 'aluminum-magnesium hydroxide',
                                                    `maxalt`= 'rizatriptan',
                                                    `maxitrol` = 'polymyxin b',
                                                    `metadate` = 'methylphenidate',
                                                    `methylin` = 'methylphenidate',
                                                    `miralax` = 'polyethylene glycol',
                                                    `mirapex` = 'pramipexole',
                                                    `motrin` = 'ibuprofen',
                                                    `mucinex` = 'guaifenesin',
                                                    `nasonex` = 'mometasone',
                                                    `nexium` = 'esomeprazole',
                                                    `norditropic` = 'somatropin',
                                                    `novolog` = 'insulin',
                                                    `omnicef` = 'cefdinir',
                                                    `omnitrope` = 'somatropin',
                                                    `pataday` = 'olopatadine',
                                                    `pepto-bismol` = 'bismuth subsalicylate',
                                                    `prevacid` = 'lansoprazole',
                                                    `prevident` = 'sodium fluoride',
                                                    `proair` = 'albuterol',
                                                    `procentra` = 'dextroamphetamine',
                                                    `protonix` = 'pantoprazole',
                                                    `protopic` = 'tacrolimus',
                                                    `proventil` = 'albuterol',
                                                    `prozac` = 'fluoxetine',
                                                    `pulmicort` = 'budesonide',
                                                    `quillichew` = 'methylphenidate',
                                                    `quillivant` = 'methylphenidate',
                                                    `qvar` = 'beclomethasone',
                                                    `retin-a` = 'tretinoin',
                                                    `rezyst probiotic` = 'bifidobacterium and lactobacillus',
                                                    `risperdal` = 'risperidone',
                                                    `ritalin` = 'methylphenidate',
                                                    `robafen` = 'guaifenesin',
                                                    `seroquel` = 'quetiapine',
                                                    `singulair` = 'montelukast',
                                                    `strattera` = 'atomoxetine',
                                                    `sublingual` = 'lorazepam',
                                                    `sucraid` = 'sacrosidase',
                                                    `sudafed` = 'pseudoephedrine',
                                                    `symbicort` = 'dudesonide-formoterol',
                                                    `synthroid` = 'levothyroxine',
                                                    `t antibiotic` = 'amoxicillin',
                                                    `tamiflu` = 'oseltamivir',
                                                    `tegretol` = 'carbamazepine',
                                                    `theraflu` = 'acetaminophen',
                                                    `thyroxine` = 'levothyroxine',
                                                    `trileptal` = 'oxcarbazepine',
                                                    `tums` = 'calcium carbonate',
                                                    `tylenol` = 'acetaminophen',
                                                    `ventolin` = 'albuterol',
                                                    `veramyst` = 'fluticasone',
                                                    `vimpat` = 'lacosamide',
                                                    `vivarin` = 'caffeine',
                                                    `vivotif` = 'typhoid vaccine',
                                                    `vyvanse` = 'lisdexamfetamine',
                                                    `wal-dryl-d` = 'diphenhydramine',
                                                    `wellbutrin` = 'bupropion',
                                                    `xopenex` = 'levalbuterol',
                                                    `xyzal` = 'levocetirizine',
                                                    `zantac` = 'ranitidine',
                                                    `zarontin` = 'ethosuximide',
                                                    `zithromax` = 'azithromycin',
                                                    `zofran` = 'ondansetron',
                                                    `zoloft` = 'sertraline',
                                                    `zyrtec` = 'cetirizine',
))

unique(sort(medrx_names$med1_rxnorm_p)) #### 189

## COLUMN 2 ##

unique(sort(medrx_names$med2_rxnorm_p)) #222
#fix inconsistent annotation
medrx_names <- medrx_names %>% mutate(med2_rxnorm_p = recode(med2_rxnorm_p,
                                                   `acetaminophen    butalbital` = 'acetaminophen',
                                                   `acetic acid  hydrocortisone otic` = 'hydrocortisone',
                                                   `albuterol   metered dose inhaler` = 'albuterol', 
                                                   `albuterol    ipratropium   inhalant` = 'albuterol',
                                                   `albuterol  ambroxol` = 'albuterol',
                                                   `albuterol  beclomethasone` = 'albuterol',
                                                   `albuterol metered dose inhaler` = 'albuterol',               
                                                   `albuterol sulfate` = 'albuterol',
                                                   `ambroxol  clenbuterol` = 'ambroxol clenbuterol',
                                                   `amoxicillin  clavulanate` = 'amoxicillin',
                                                   `amphetamine  dextroamphetamine` = 'amphetamine',
                                                   `azelastine hydrochloride` = 'azelastine',
                                                   `beclomethasone dipropionate   metered dose inhaler` = 'beclomethasone',
                                                   `benadryl-d allergy and sinus` = 'benadryl',
                                                   `budesonide inhalant` = 'budesonide',
                                                   `buspirone hydrochloride` = 'buspirone',
                                                   `cetirizine  pseudoephedrine` = 'cetirizine',
                                                   `cetirizine hydrochloride` = 'cetirizine',
                                                   `claritin-d` = 'claritin',
                                                   `clonidine hydrochloride` = 'clonidine',
                                                   `cortizone-` = 'cortizone',
                                                   `dayquil hbp` = 'dayquil',
                                                   `desmopressin acetate` = 'desmopressin',
                                                   `dexmethylphenidate hydrochloride` = 'dexmethylphenidate',
                                                   `epinephrine    lidocaine hydrochloride` = 'epinephrine',
                                                   `epinephrine inhalant` = 'epinephrine',
                                                   `ferrous sulfate` = 'ferrous',
                                                   `fluticasone  vilanterol` = 'fluticasone',
                                                   `fluticasone metered dose` = 'fluticasone',
                                                   `fluticasone propionate` = 'fluticasone',
                                                   `fluticasone propionate   metered dose` = 'fluticasone',
                                                   `fluticasone propionate   metered dose inhaler` = 'fluticasone',
                                                   `fluvoxamine maleate` = 'fluvoxamine',
                                                   `guanfacine hydrochloride` = 'guanfacine',
                                                   `hr dexmethylphenidate hydrochloride` = 'dexmethylphenidate',
                                                   `hydroxyzine hydrochloride` = 'hydroxyzine',
                                                   `hydroxyzine pamoate` = 'hydroxyzine',
                                                   `insulin glargine  unt pen injector` = 'insulin',
                                                   `ketoconazole    foam` = 'ketoconazole',
                                                   `ketotifen   ophthalmic` = 'ketotifen',
                                                   `levalbuterol tartrate` = 'levalbuterol',
                                                   `levothyroxine sodium` = 'levothyroxine',
                                                   `melatonin    theanine` = 'melatonin',
                                                   `methylphenidate hydrochloride` = 'methylphenidate',
                                                   `microsporum audouinii allergenic extract` = 'microsporum audouinii',
                                                   `mometasone furoate` = 'mometasone',
                                                   `montelukast sodium` = 'montelukast',
                                                   `nyquil cough` = 'nyquil',
                                                   `olopatadine   ophthalmic` = 'olopatadine',
                                                   `olopatadine hydrochloride` = 'olopatadine',
                                                   `oxybutynin chloride` = 'oxybutynin',
                                                   `polyethylene glycol     potassium chloride    sodium bicarbonate    sodium chloride   powder for` = 'polyethylene glycol',
                                                   `quetiapine fumarate` = 'quetiapine',
                                                   `release  hr methylphenidate hydrochloride` = 'methylphenidate',
                                                   `strattera  kit` = 'strattera',
                                                   `triaminic day time cold & cough` = 'triaminic',
                                                   `vicks vaporub` = 'vicks',
                                                   `vitamin c tr` = 'vitamin c',
                                                   `zyrtec-d` = 'zyrtec',
))

unique(sort(medrx_names$med2_rxnorm_p)) #### 182
#brand to generic 
medrx_names <- medrx_names %>% mutate(med2_rxnorm_p = recode(med2_rxnorm_p,
                                                    `abilify` = 'aripiprazole',
                                                    `adderall` = 'dextroamphetamine',
                                                    `advair` = 'fluticasone',
                                                    `adzenys` = 'amphetamine',
                                                    `afrin` = 'oxymetazoline',
                                                    `allegra` = 'fexofenadine',
                                                    `alvesco` = 'ciclesonide',
                                                    `aptensio` = 'methylphenidate',
                                                    `auvi-q` = 'epinephrine',
                                                    `benadryl` = 'diphenhydramine',
                                                    `claritin` = 'loratadine',
                                                    `concerta`= 'methylphenidate',
                                                    `cortizone` = 'hydrocortisone',
                                                    `dayquil` = 'acetaminophen',
                                                    `decadron` = 'dexamethasone',
                                                    `delsym` = 'dextromethorphan',
                                                    `depakote` = 'divalproex',
                                                    `dulera` = 'mometasone',
                                                    `dyanavel` = 'amphetamine',
                                                    `dymista` = 'azelastine-fluticasone',
                                                    `entocort` = 'budesonide',
                                                    `epipen` = 'epinephrine',
                                                    `flonase` = 'fluticasone',
                                                    `flovent` = 'fluticasone', 
                                                    `focalin` = 'dexmethylphenidate',
                                                    `humalog` = 'insulin',
                                                    `imitrex` = 'sumatriptan',
                                                    `intuniv` = 'guanfacine',
                                                    `jadenu` = 'deferasirox',
                                                    `lac-hydrin` = 'ammonium lactate',
                                                    `lamictal` = 'lamotrigine',
                                                    `lantus` = 'insulin',
                                                    `latuda` = 'lurasidone',
                                                    `levemir` = 'insulin',
                                                    `lexapro` = 'escitalopram',
                                                    `little colds mucus relief` = 'guaifenesin',
                                                    `ludent` = 'sodium fluoride',
                                                    `macrodantin` = 'nitrofurantoin',
                                                    `metadate`= 'methylphenidate',
                                                    `miralax` = 'polyethylene glycol',
                                                    `nasacort` = 'triamcinolone',
                                                    `nasonex` = 'mometasone',
                                                    `neosporin` = 'neomycin-bacitracnzn-polymyxnb',
                                                    `norditropin` = 'somatropin',
                                                    `novolin n` = 'insulin',
                                                    `novolog` = 'insulin',
                                                    `nyquil` = 'acetaminophen',
                                                    `prevacid` = 'lansoprazole',
                                                    `proair` = 'albuterol',
                                                    `prograf` = 'tacrolimus',
                                                    `proventil` = 'albuterol',
                                                    `prozac` = 'fluoxetine',
                                                    `pulmicort` = 'budesonide',
                                                    `quillivant` = 'methylphenidate',
                                                    `qvar` = 'beclomethasone',
                                                    `remeron` = 'mirtazapine',
                                                    `remicade` = 'infliximab',
                                                    `restasis` = 'cyclosporine',
                                                    `rhinocort` = 'budesonide',
                                                    `risperdal` = 'risperidone',
                                                    `ritalin` = 'methylphenidate',
                                                    `seroquel` = 'quetiapine',
                                                    `singulair` = 'montelukast',
                                                    `strattera` = 'atomoxetine',
                                                    `sublingual` = 'lorazepam',
                                                    `symbicort` = 'dudesonide-formoterol',
                                                    `tamiflu` = 'oseltamivir',
                                                    `tenex` = 'guanfacine',
                                                    `theraflu` = 'acetaminophen',
                                                    `triaminic` = 'chlorpheniramine',
                                                    `tylenol` = 'acetaminophen',
                                                    `ventolin` = 'albuterol',
                                                    `veramyst` = 'fluticasone',
                                                    `vicks` = 'camphor-eucalyptus oil-menthol',
                                                    `vivelle transdermal` = 'estradiol',
                                                    `vyvanse` = 'lisdexamfetamine',
                                                    `wal-dryl-d` = 'diphenhydramine',
                                                    `xanax` = 'alprazolam',
                                                    `zantac` = 'ranitidine',
                                                    `zarontin` = 'ethosuximide',
                                                    `zithromax` = 'azithromycin',
                                                    `zofran` = 'ondansetron',
                                                    `zoloft` = 'sertraline',
                                                    `zyrtec` = 'cetirizine'
))

unique(sort(medrx_names$med2_rxnorm_p)) #### 125

## COLUMN 3 ##

unique(sort(medrx_names$med3_rxnorm_p)) #139
#fix inconsistent annotation
medrx_names <- medrx_names %>% mutate(med3_rxnorm_p = recode(med3_rxnorm_p,
                                                   `albuterol   metered dose inhaler` = 'albuterol', 
                                                   `albuterol  ipratropium` = 'albuterol',
                                                   `albuterol  beclomethasone` = 'albuterol',
                                                   `albuterol metered dose inhaler` = 'albuterol',               
                                                   `albuterol sulfate` = 'albuterol',
                                                   `amitriptyline hydrochloride` = 'amitriptyline',
                                                   `beclomethasone dipropionate` = 'beclomethasone',
                                                   `buspirone hydrochloride` = 'buspirone',
                                                   `cetirizine hydrochloride` = 'cetirizine',
                                                   `claritin-d` = 'claritin',
                                                   `clonidine hydrochloride` = 'clonidine',
                                                   `desmopressin acetate` = 'desmopressin',
                                                   `dexamethasone  neomycin  polymyxin b` = 'neomycin-bacitracnzn-polymyxnb',
                                                   `dextroamphetamine sulfate` = 'dextroamphetamine',
                                                   `ferrous sulfate` = 'ferrous',
                                                   `fexofenadine hydrochloride` = 'fexofenadine',
                                                   `fluoride ion` = 'fluoride',
                                                   `fluticasone  salmeterol` = 'fluticasone',
                                                   `fluticasone propionate` = 'fluticasone',
                                                   `fluticasone propionate    salmeterol xinafoate` = 'fluticasone',
                                                   `fluticasone propionate   metered dose inhaler` = 'fluticasone',
                                                   `fluticasone propionate   metered dose` = 'fluticasone',
                                                   `guafacine hydrochloride` = 'guanfacine',
                                                   `hr dexmethylphenidate hydrochloride` = 'dexmethylphenidate',
                                                   `hr guanfacine` = 'guanfacine',
                                                   `levalbuterol inhalant` = 'levalbuterol',
                                                   `levothyroxine sodium` = 'levothyroxine',
                                                   `methylphenidate hydrochloride` = 'methylphenidate',
                                                   `miralax  powder` = 'miralax',
                                                   `oseltamivir phosphate` = 'oseltamivir',
                                                   `polyethylene glycols` = 'polyethylene glycol',
                                                   `t vitamin` = 'vitamin t',
                                                   `tobramycin sulfate` = 'tobramycin',
                                                   `triamcinolone acetonide` = 'triamcinolone'
))

unique(sort(medrx_names$med3_rxnorm_p)) #### 119
#brand to generic 
medrx_names <- medrx_names %>% mutate(med3_rxnorm_p = recode(med3_rxnorm_p,
                                                    `abilify` = 'aripiprazole',
                                                    `adderall` = 'dextroamphetamine',
                                                    `advair` = 'fluticasone',
                                                    `aerospan` = 'flunisolide',
                                                    `allegra` = 'fexofenadine',
                                                    `aptensio` = 'methylphenidate',
                                                    `ciprodex` = 'ciprofloxacin/dexamethasone',
                                                    `claritin` = 'loratadine',
                                                    `concerta`= 'methylphenidate',
                                                    `creon` = 'pancrelipase',
                                                    `cyproheptadine` = 'periactin',
                                                    `daypro` = 'oxaprozin',
                                                    `ddavp` = 'desmopressin',
                                                    `depakote` = 'divalproex',
                                                    `dulera` = 'formoterol',
                                                    `dymista` = 'azelastine-fluticasone',
                                                    `flonase` = 'fluticasone',
                                                    `flovent` = 'fluticasone', 
                                                    `focalin` = 'dexmethylphenidate',
                                                    `iron` = 'ferrous',
                                                    `intuniv` = 'guanfacine',
                                                    `lexapro` = 'escitalopram',
                                                    `miralax` = 'polyethylene glycol',
                                                    `motrin` = 'ibuprofen',
                                                    `mucinex` = 'guaifenesin',
                                                    `nasonex` = 'mometasone',
                                                    `novolog` = 'insulin',
                                                    `pazeo` = 'olopatadine',
                                                    `pepcid`= 'famotidine',
                                                    `phenargan` = 'promethazine',
                                                    `prevacid` = 'lansoprazole',
                                                    `prilosec` = 'omeprazole',
                                                    `proair` = 'albuterol',
                                                    `proventil` = 'albuterol',
                                                    `prozac` = 'fluoxetine',
                                                    `pulmicort` = 'budesonide',
                                                    `qvar` = 'beclomethasone',
                                                    `risperdal` = 'risperidone',
                                                    `ritalin` = 'methylphenidate',
                                                    `sedum roseum root extract` = 'rhodiola rosea',
                                                    `singulair` = 'montelukast',
                                                    `strattera` = 'atomoxetine',
                                                    `tenex` = 'guanfacine',
                                                    `thyroxine` = 'levothyroxine',
                                                    `trileptal` = 'oxcarbazepine',
                                                    `ventolin` = 'albuterol',
                                                    `veramyst` = 'fluticasone',
                                                    `vyvanse` = 'lisdexamfetamine',
                                                    `xopenex` = 'levalbuterol',
                                                    `zaditor` = 'ketotifen',
                                                    `zyrtec` = 'cetirizine'
))

unique(sort(medrx_names$med3_rxnorm_p)) #### 87

## COLUMN 4 ##

unique(sort(medrx_names$med4_rxnorm_p)) #86
#fix inconsistent annotation
medrx_names <- medrx_names %>% mutate(med4_rxnorm_p = recode(med4_rxnorm_p,
                                                   `albuterol  cromolyn inhalant` = 'albuterol',            
                                                   `albuterol sulfate` = 'albuterol',
                                                   `amoxicillin  clavulanate` = 'amoxicillin',
                                                   `beclomethasone dipropionate   metered dose inhaler` = 'beclomethasone',
                                                   `benadryl itch stopping` = 'benadryl',
                                                   `cetirizine hydrochloride` = 'cetirizine',
                                                   `clonidine hydrochloride` = 'clonidine',
                                                   `cyproheptadine hydrochloride` = 'cyproheptadine',
                                                   `divalproex sodium` = 'divalproex',
                                                   `fluticasone propionate` = 'fluticasone',
                                                   `guanfacine hydrochloride` = 'guanfacine',
                                                   `hydroxyzine pamoate` = 'hydroxyzine',
                                                   `mometasone furoate` = 'mometasone',
                                                   `montelukast sodium` = 'montelukast',
                                                   `polyethylene glycol     potassium chloride    sodium bicarbonate    sodium chloride   powder for` = 'polyethylene glycol',
                                                   `t vitamin` = 'vitamin t',
                                                   `trazodone hydrochloride` = 'trazodone',
                                                   `vitamin c tr` = 'vitamin c'
))

unique(sort(medrx_names$med4_rxnorm_p)) #### 74
#brand to generic 
medrx_names <- medrx_names %>% mutate(med4_rxnorm_p = recode(med4_rxnorm_p,
                                                    `abilify` = 'aripiprazole',
                                                    `adderall` = 'dextroamphetamine',
                                                    `advair` = 'fluticasone',
                                                    `aspirin` = 'acetylsalicylic acid',
                                                    `astelin` = 'azelastine',
                                                    `benadryl` = 'diphenhydramine',
                                                    `claritin` = 'loratadine',
                                                    `creon` = 'pancrelipase',
                                                    `cyproheptadine` = 'periactin',
                                                    `dulera` = 'formoterol',
                                                    `epipen` = 'epinephrine',
                                                    `flonase` = 'fluticasone',
                                                    `flovent` = 'fluticasone', 
                                                    `focalin` = 'dexmethylphenidate',
                                                    `miralax` = 'polyethylene glycol',
                                                    `mucinex` = 'guaifenesin',
                                                    `nasonex` = 'mometasone',
                                                    `proair` = 'albuterol',
                                                    `prozac` = 'fluoxetine',
                                                    `qvar` = 'beclomethasone',
                                                    `remeron` = 'mirtazapine',
                                                    `ritalin` = 'methylphenidate',
                                                    `singulair` = 'montelukast',
                                                    `symbicort` = 'budesonide',
                                                    `ventolin` = 'albuterol',
                                                    `vyvanse` = 'lisdexamfetamine',
                                                    `zenpep` = 'pancrelipase',
                                                    `zoloft` = 'sertraline',
                                                    `zyrtec` = 'cetirizine'
))

unique(sort(medrx_names$med4_rxnorm_p)) #### 56

## COLUMN 5 ##

unique(sort(medrx_names$med5_rxnorm_p)) #52
#fix inconsistent annotation
medrx_names <- medrx_names %>% mutate(med5_rxnorm_p = recode(med5_rxnorm_p,
                                                   `albuterol   metered dose inhaler` = 'albuterol',
                                                   `albuterol  beclomethasone` = 'albuterol',
                                                   `albuterol sulfate` = 'albuterol',
                                                   `budesonide   inhalant` = 'budesonide',
                                                   `dornase alfa   inhalant` = 'dornase',
                                                   `lisdexamfetamine dimesylate` = 'lisdexamfetamine',
                                                   `sulfamethizole` = 'sulfamethoxazole',
                                                   `trazodone hydrochloride` = 'trazodone',
))

unique(sort(medrx_names$med5_rxnorm_p)) #### 47
#brand to generic 
medrx_names <- medrx_names %>% mutate(med5_rxnorm_p = recode(med5_rxnorm_p,
                                                    `adderall` = 'dextroamphetamine',
                                                    `advair` = 'fluticasone',
                                                    `alvesco` = 'ciclesonide',
                                                    `ciprodex` = 'ciprofloxacin/dexamethasone',
                                                    `cromolyn` = 'albuterol',
                                                    `cyproheptadine` = 'periactin',
                                                    `dulera` = 'formoterol',
                                                    `flonase` = 'fluticasone',
                                                    `flovent` = 'fluticasone', 
                                                    `focalin` = 'dexmethylphenidate',
                                                    `intuniv` = 'guanfacine',
                                                    `mapap` = 'acetaminophen',
                                                    `nasonex` = 'mometasone',
                                                    `nexium` = 'esomeprazole',
                                                    `oleptro` = 'trazodone',
                                                    `prozac` = 'fluoxetine',
                                                    `pulmicort` = 'budesonide',
                                                    `qvar` = 'beclomethasone',
                                                    `tussionex` = 'hydrocodone-chlorpheniramine',
                                                    `ventolin` = 'albuterol',
                                                    `zyrtec` = 'cetirizine'
))

unique(sort(medrx_names$med5_rxnorm_p)) #### 36

## COLUMN 6 ##

unique(sort(medrx_names$med6_rxnorm_p)) #27
#fix inconsistent annotation
medrx_names <- medrx_names %>% mutate(med6_rxnorm_p = recode(med6_rxnorm_p,
                                                   `albuterol   metered dose inhaler` = 'albuterol',
                                                   `cetirizine hydrochloride` = 'cetirizine',
                                                   `hydroxyzine pamoate` = 'hydroxyzine'
))

unique(sort(medrx_names$med6_rxnorm_p)) #### 26
#brand to generic 
medrx_names <- medrx_names %>% mutate(med6_rxnorm_p = recode(med6_rxnorm_p,
                                                    `adderall` = 'dextroamphetamine',
                                                    `benadryl` = 'diphenhydramine',
                                                    `dulera` = 'formoterol',
                                                    `flomax` = 'tamsulosin',
                                                    `flovent` = 'fluticasone',
                                                    `proair` = 'albuterol',
                                                    `pulmozyme` = 'dornase',
                                                    `singulair` = 'montelukast',
                                                    `vyvanse` = 'lisdexamfetamine'
))

unique(sort(medrx_names$med6_rxnorm_p)) #### 23

## COLUMN 7 ##

unique(sort(medrx_names$med7_rxnorm_p)) #18
#fix inconsistent annotation
medrx_names <- medrx_names %>% mutate(med7_rxnorm_p = recode(med7_rxnorm_p,
                                                   `albuterol sulfate` = 'albuterol',
                                                   `hydrocortisone acetate` = 'hydrocortisone',
                                                   `t vitamin` = 'vitamin t',
                                                   `z-pak  count pack` = 'z-pak'
))

unique(sort(medrx_names$med7_rxnorm_p)) #### 18
#brand to generic 
medrx_names <- medrx_names %>% mutate(med7_rxnorm_p = recode(med7_rxnorm_p,
                                                    `flovent` = 'fluticasone',
                                                    `orkambi` = 'ivacaftor/lumacaftor',
                                                    `qvar` = 'beclomethasone',
                                                    `singulair` = 'montelukast',
                                                    `strattera` = 'atomoxetine',
                                                    `z-pak` = 'azithromycin',
                                                    `zantac` = 'ranitidine',
                                                    `zyrtec` = 'cetirizine'
))

unique(sort(medrx_names$med7_rxnorm_p)) #### 16

## COLUMN 8 ##

unique(sort(medrx_names$med8_rxnorm_p)) #13
#brand to generic 
medrx_names <- medrx_names %>% mutate(med8_rxnorm_p = recode(med8_rxnorm_p,
                                                    `clarinex-d` = 'desloratadine',
                                                    `concerta`= 'methylphenidate',
                                                    `orkambi` = 'ivacaftor/lumacaftor',
                                                    `proair` = 'albuterol',
                                                    `ventolin` = 'albuterol'
))

unique(sort(medrx_names$med8_rxnorm_p)) #### 10

## COLUMN 9 ##

unique(sort(medrx_names$med9_rxnorm_p)) #9
#fix inconsistent annotation
medrx_names <- medrx_names %>% mutate(med9_rxnorm_p = recode(med9_rxnorm_p,
                                                   `polyethylene glycol   potassium chloride  sodium bicarbonate  sodium chloride  sodium sulfate` = 'polyethylene glycol',
                                                   `sulfamethoxazole  trimethoprim` = 'sulfamethoxazole trimethoprim',
                                                   `t vitamin` = 'vitamin t'
))

unique(sort(medrx_names$med9_rxnorm_p)) #9
#brand to generic 
medrx_names <- medrx_names %>% mutate(med9_rxnorm_p = recode(med9_rxnorm_p,
                                                    `advair` = 'fluticasone',
                                                    `flonase` = 'fluticasone'
))

unique(sort(medrx_names$med9_rxnorm_p)) #### 8

## COLUMN 10 ##

unique(sort(medrx_names$med10_rxnorm_p)) #9
#fix inconsistent annotation
medrx_names <- medrx_names %>% mutate(med10_rxnorm_p = recode(med10_rxnorm_p,
                                                   `cetirizine hydrochloride` = 'cetirizine',
                                                   `clonidine hydrochloride` = 'clonidine'
))

unique(sort(medrx_names$med10_rxnorm_p)) #9
#brand to generic 
medrx_names <- medrx_names %>% mutate(med10_rxnorm_p = recode(med10_rxnorm_p,
                                                    `duoneb` = 'albuterol',
                                                    `proair` = 'albuterol'
))

unique(sort(medrx_names$med10_rxnorm_p)) #### 8

## COLUMN 11 ##

unique(sort(medrx_names$med11_rxnorm_p)) #6
#fix inconsistent annotation
medrx_names <- medrx_names %>% mutate(med11_rxnorm_p = recode(med11_rxnorm_p,
                                                   `montelukast sodium` = 'montelukast',
                                                   `tobi podhaler   monthly pack` = 'tobi podhaler',
                                                   `triamcinolone acetonide` = 'triamcinolone'
))

unique(sort(medrx_names$med11_rxnorm_p)) #6
#brand to generic 
medrx_names <- medrx_names %>% mutate(med11_rxnorm_p = recode(med11_rxnorm_p,
                                                    `tobi podhaler` = 'tobramycin',
                                                    `xopenex` = 'levalbuterol'
))

unique(sort(medrx_names$med11_rxnorm_p)) #### 6

## COLUMN 12 ##

unique(sort(medrx_names$med12_rxnorm_p)) #6
#fix inconsistent annotation
medrx_names <- medrx_names %>% mutate(med12_rxnorm_p = recode(med12_rxnorm_p,
                                                   `albuterol   metered dose inhaler` = 'albuterol',
                                                   `t vitamin` = 'vitamin t'
))

unique(sort(medrx_names$med12_rxnorm_p)) #6
#brand to generic 
medrx_names <- medrx_names %>% mutate(med12_rxnorm_p = recode(med12_rxnorm_p,
                                                    `pataday` = 'olopatadine',
                                                    `qvar` = 'beclomethasone',
                                                    `xopenex` = 'levalbuterol'
))

unique(sort(medrx_names$med12_rxnorm_p)) #### 6

## COLUMN 13 ##

unique(sort(medrx_names$med13_rxnorm_p)) #3

## COLUMN 14 ##

unique(sort(medrx_names$med14_rxnorm_p)) #2
#brand to generic 
medrx_names <- medrx_names %>% mutate(med14_rxnorm_p = recode(med14_rxnorm_p,
                                                    `benadryl` = 'diphenhydramine'
))

## COLUMN 15 ##

unique(sort(medrx_names$med15_rxnorm_p)) #1
#brand to generic 
medrx_names <- medrx_names %>% mutate(med15_rxnorm_p = recode(med15_rxnorm_p,
                                                    `zantac` = 'ranitidine'
))

#################################################################
###               ORGANIZING BY DRUG CATEGORIES               ###
#################################################################

#names of all meds
names <- as.vector(as.matrix(medrx_names[,2:16]))
unique(sort(names)) #### 250

#want how many of each unique values there are (frequency)
freq <- as.data.frame(table(names))

#depressants = 1
medrx_names$depressants <- NA

medrx_names$depressants [medrx_names$med1_rxnorm_p== "acetazolamide"| medrx_names$med1_rxnorm_p== "alprazolam"| 
                             medrx_names$med1_rxnorm_p== "aripiprazole"| medrx_names$med1_rxnorm_p== "atomoxetine" |
                             medrx_names$med1_rxnorm_p== "bupropion"|medrx_names$med1_rxnorm_p== "carbamazepine"|
                             medrx_names$med1_rxnorm_p== "citalopram"|medrx_names$med1_rxnorm_p== "clonazepam"|
                             medrx_names$med1_rxnorm_p== "clonidine"|medrx_names$med1_rxnorm_p== "clozapine"|
                             medrx_names$med1_rxnorm_p== "diazepam"|medrx_names$med1_rxnorm_p== "divalproex"|
                             medrx_names$med1_rxnorm_p== "escitalopram"|
                             medrx_names$med1_rxnorm_p== "ethosuximide"|medrx_names$med1_rxnorm_p== "fluoxetine"|
                             medrx_names$med1_rxnorm_p== "fluvoxamine"|medrx_names$med1_rxnorm_p== "gabapentin"|
                             medrx_names$med1_rxnorm_p== "hydroxyzine"|medrx_names$med1_rxnorm_p== "lacosamide"|
                             medrx_names$med1_rxnorm_p== "lamotrigine"|medrx_names$med1_rxnorm_p== "levetiracetam"|
                             medrx_names$med1_rxnorm_p== "levocetirizine"|medrx_names$med1_rxnorm_p== "lorazepam"|
                             medrx_names$med1_rxnorm_p== "lurasidone"|medrx_names$med1_rxnorm_p== "melatonin"|
                             medrx_names$med1_rxnorm_p== "mirtazapine"|medrx_names$med1_rxnorm_p== "ondansetron"|
                             medrx_names$med1_rxnorm_p== "oxcarbazepine"|medrx_names$med1_rxnorm_p== "quetiapine"|
                             medrx_names$med1_rxnorm_p== "risperidone"|medrx_names$med1_rxnorm_p== "rizatriptan"|
                             medrx_names$med1_rxnorm_p== "sertraline"|medrx_names$med1_rxnorm_p== "sumatriptan"|
                             medrx_names$med1_rxnorm_p== "topiramate"|medrx_names$med1_rxnorm_p== "trazodone"|
                             medrx_names$med1_rxnorm_p== "ziprasidone"|medrx_names$med1_rxnorm_p== "zonisamide"] <- 1

medrx_names$depressants [medrx_names$med2_rxnorm_p== "acetazolamide"| medrx_names$med2_rxnorm_p== "alprazolam"| 
                             medrx_names$med2_rxnorm_p== "aripiprazole"| medrx_names$med2_rxnorm_p== "atomoxetine" |
                             medrx_names$med2_rxnorm_p== "bupropion"|medrx_names$med2_rxnorm_p== "carbamazepine"|
                             medrx_names$med2_rxnorm_p== "citalopram"|medrx_names$med2_rxnorm_p== "clonazepam"|
                             medrx_names$med2_rxnorm_p== "clonidine"|medrx_names$med2_rxnorm_p== "clozapine"|
                             medrx_names$med2_rxnorm_p== "diazepam"|medrx_names$med2_rxnorm_p== "divalproex"|
                             medrx_names$med2_rxnorm_p== "escitalopram"|
                             medrx_names$med2_rxnorm_p== "ethosuximide"|medrx_names$med2_rxnorm_p== "fluoxetine"|
                             medrx_names$med2_rxnorm_p== "fluvoxamine"|medrx_names$med2_rxnorm_p== "gabapentin"|
                             medrx_names$med2_rxnorm_p== "hydroxyzine"|medrx_names$med2_rxnorm_p== "lacosamide"|
                             medrx_names$med2_rxnorm_p== "lamotrigine"|medrx_names$med2_rxnorm_p== "levetiracetam"|
                             medrx_names$med2_rxnorm_p== "levocetirizine"|medrx_names$med2_rxnorm_p== "lorazepam"|
                             medrx_names$med2_rxnorm_p== "lurasidone"|medrx_names$med2_rxnorm_p== "melatonin"|
                             medrx_names$med2_rxnorm_p== "mirtazapine"|medrx_names$med2_rxnorm_p== "ondansetron"|
                             medrx_names$med2_rxnorm_p== "oxcarbazepine"|medrx_names$med2_rxnorm_p== "quetiapine"|
                             medrx_names$med2_rxnorm_p== "risperidone"|medrx_names$med2_rxnorm_p== "rizatriptan"|
                             medrx_names$med2_rxnorm_p== "sertraline"|medrx_names$med2_rxnorm_p== "sumatriptan"|
                             medrx_names$med2_rxnorm_p== "topiramate"|medrx_names$med2_rxnorm_p== "trazodone"|
                             medrx_names$med2_rxnorm_p== "ziprasidone"|medrx_names$med2_rxnorm_p== "zonisamide"] <- 1

medrx_names$depressants [medrx_names$med3_rxnorm_p== "acetazolamide"| medrx_names$med3_rxnorm_p== "alprazolam"| 
                             medrx_names$med3_rxnorm_p== "aripiprazole"| medrx_names$med3_rxnorm_p== "atomoxetine" |
                             medrx_names$med3_rxnorm_p== "bupropion"|medrx_names$med3_rxnorm_p== "carbamazepine"|
                             medrx_names$med3_rxnorm_p== "citalopram"|medrx_names$med3_rxnorm_p== "clonazepam"|
                             medrx_names$med3_rxnorm_p== "clonidine"|medrx_names$med3_rxnorm_p== "clozapine"|
                             medrx_names$med3_rxnorm_p== "diazepam"|medrx_names$med3_rxnorm_p== "divalproex"|
                             medrx_names$med3_rxnorm_p== "escitalopram"|
                             medrx_names$med3_rxnorm_p== "ethosuximide"|medrx_names$med3_rxnorm_p== "fluoxetine"|
                             medrx_names$med3_rxnorm_p== "fluvoxamine"|medrx_names$med3_rxnorm_p== "gabapentin"|
                             medrx_names$med3_rxnorm_p== "hydroxyzine"|medrx_names$med3_rxnorm_p== "lacosamide"|
                             medrx_names$med3_rxnorm_p== "lamotrigine"|medrx_names$med3_rxnorm_p== "levetiracetam"|
                             medrx_names$med3_rxnorm_p== "levocetirizine"|medrx_names$med3_rxnorm_p== "lorazepam"|
                             medrx_names$med3_rxnorm_p== "lurasidone"|medrx_names$med3_rxnorm_p== "melatonin"|
                             medrx_names$med3_rxnorm_p== "mirtazapine"|medrx_names$med3_rxnorm_p== "ondansetron"|
                             medrx_names$med3_rxnorm_p== "oxcarbazepine"|medrx_names$med3_rxnorm_p== "quetiapine"|
                             medrx_names$med3_rxnorm_p== "risperidone"|medrx_names$med3_rxnorm_p== "rizatriptan"|
                             medrx_names$med3_rxnorm_p== "sertraline"|medrx_names$med3_rxnorm_p== "sumatriptan"|
                             medrx_names$med3_rxnorm_p== "topiramate"|medrx_names$med3_rxnorm_p== "trazodone"|
                             medrx_names$med3_rxnorm_p== "ziprasidone"|medrx_names$med3_rxnorm_p== "zonisamide"] <- 1

medrx_names$depressants [medrx_names$med4_rxnorm_p== "acetazolamide"| medrx_names$med4_rxnorm_p== "alprazolam"| 
                             medrx_names$med4_rxnorm_p== "aripiprazole"| medrx_names$med4_rxnorm_p== "atomoxetine" |
                             medrx_names$med4_rxnorm_p== "bupropion"|medrx_names$med4_rxnorm_p== "carbamazepine"|
                             medrx_names$med4_rxnorm_p== "citalopram"|medrx_names$med4_rxnorm_p== "clonazepam"|
                             medrx_names$med4_rxnorm_p== "clonidine"|medrx_names$med4_rxnorm_p== "clozapine"|
                             medrx_names$med4_rxnorm_p== "diazepam"|medrx_names$med4_rxnorm_p== "divalproex"|
                             medrx_names$med4_rxnorm_p== "escitalopram"|
                             medrx_names$med4_rxnorm_p== "ethosuximide"|medrx_names$med4_rxnorm_p== "fluoxetine"|
                             medrx_names$med4_rxnorm_p== "fluvoxamine"|medrx_names$med4_rxnorm_p== "gabapentin"|
                             medrx_names$med4_rxnorm_p== "hydroxyzine"|medrx_names$med4_rxnorm_p== "lacosamide"|
                             medrx_names$med4_rxnorm_p== "lamotrigine"|medrx_names$med4_rxnorm_p== "levetiracetam"|
                             medrx_names$med4_rxnorm_p== "levocetirizine"|medrx_names$med4_rxnorm_p== "lorazepam"|
                             medrx_names$med4_rxnorm_p== "lurasidone"|medrx_names$med4_rxnorm_p== "melatonin"|
                             medrx_names$med4_rxnorm_p== "mirtazapine"|medrx_names$med4_rxnorm_p== "ondansetron"|
                             medrx_names$med4_rxnorm_p== "oxcarbazepine"|medrx_names$med4_rxnorm_p== "quetiapine"|
                             medrx_names$med4_rxnorm_p== "risperidone"|medrx_names$med4_rxnorm_p== "rizatriptan"|
                             medrx_names$med4_rxnorm_p== "sertraline"|medrx_names$med4_rxnorm_p== "sumatriptan"|
                             medrx_names$med4_rxnorm_p== "topiramate"|medrx_names$med4_rxnorm_p== "trazodone"|
                             medrx_names$med4_rxnorm_p== "ziprasidone"|medrx_names$med4_rxnorm_p== "zonisamide"] <- 1

medrx_names$depressants [medrx_names$med5_rxnorm_p== "acetazolamide"| medrx_names$med5_rxnorm_p== "alprazolam"| 
                             medrx_names$med5_rxnorm_p== "aripiprazole"| medrx_names$med5_rxnorm_p== "atomoxetine" |
                             medrx_names$med5_rxnorm_p== "bupropion"|medrx_names$med5_rxnorm_p== "carbamazepine"|
                             medrx_names$med5_rxnorm_p== "citalopram"|medrx_names$med5_rxnorm_p== "clonazepam"|
                             medrx_names$med5_rxnorm_p== "clonidine"|medrx_names$med5_rxnorm_p== "clozapine"|
                             medrx_names$med5_rxnorm_p== "diazepam"|medrx_names$med5_rxnorm_p== "divalproex"|
                             medrx_names$med5_rxnorm_p== "escitalopram"|
                             medrx_names$med5_rxnorm_p== "ethosuximide"|medrx_names$med5_rxnorm_p== "fluoxetine"|
                             medrx_names$med5_rxnorm_p== "fluvoxamine"|medrx_names$med5_rxnorm_p== "gabapentin"|
                             medrx_names$med5_rxnorm_p== "hydroxyzine"|medrx_names$med5_rxnorm_p== "lacosamide"|
                             medrx_names$med5_rxnorm_p== "lamotrigine"|medrx_names$med5_rxnorm_p== "levetiracetam"|
                             medrx_names$med5_rxnorm_p== "levocetirizine"|medrx_names$med5_rxnorm_p== "lorazepam"|
                             medrx_names$med5_rxnorm_p== "lurasidone"|medrx_names$med5_rxnorm_p== "melatonin"|
                             medrx_names$med5_rxnorm_p== "mirtazapine"|medrx_names$med5_rxnorm_p== "ondansetron"|
                             medrx_names$med5_rxnorm_p== "oxcarbazepine"|medrx_names$med5_rxnorm_p== "quetiapine"|
                             medrx_names$med5_rxnorm_p== "risperidone"|medrx_names$med5_rxnorm_p== "rizatriptan"|
                             medrx_names$med5_rxnorm_p== "sertraline"|medrx_names$med5_rxnorm_p== "sumatriptan"|
                             medrx_names$med5_rxnorm_p== "topiramate"|medrx_names$med5_rxnorm_p== "trazodone"|
                             medrx_names$med5_rxnorm_p== "ziprasidone"|medrx_names$med5_rxnorm_p== "zonisamide"] <- 1

medrx_names$depressants [medrx_names$med6_rxnorm_p== "fluoxetine"|
                             medrx_names$med6_rxnorm_p== "hydroxyzine"|medrx_names$med6_rxnorm_p== "melatonin"|
                             medrx_names$med6_rxnorm_p== "mirtazapine"|medrx_names$med6_rxnorm_p== "ondansetron"|
                             medrx_names$med6_rxnorm_p== "trazodone"] <- 1

medrx_names$depressants [medrx_names$med7_rxnorm_p== "atomoxetine" |
                             medrx_names$med7_rxnorm_p== "clonidine"|medrx_names$med7_rxnorm_p== "melatonin"] <- 1

medrx_names$depressants [medrx_names$med8_rxnorm_p== "melatonin"] <- 1

medrx_names$depressants [medrx_names$med10_rxnorm_p== "clonidine"] <- 1

medrx_names$depressants [medrx_names$med13_rxnorm_p== "sertraline"] <- 1

sum(!is.na(medrx_names$depressants))

#stimulants = 2
medrx_names$stimulants <- NA

medrx_names$stimulants [medrx_names$med1_rxnorm_p== "amphetamine"|medrx_names$med1_rxnorm_p== "caffeine"|
                             medrx_names$med1_rxnorm_p== "dexmethylphenidate"|medrx_names$med1_rxnorm_p== "dextroamphetamine"|
                             medrx_names$med1_rxnorm_p== "lisdexamfetamine"|medrx_names$med1_rxnorm_p== "methylphenidate"] <- 2

medrx_names$stimulants [medrx_names$med2_rxnorm_p== "amphetamine"|medrx_names$med2_rxnorm_p== "caffeine"|
                             medrx_names$med2_rxnorm_p== "dexmethylphenidate"|medrx_names$med2_rxnorm_p== "dextroamphetamine"|
                             medrx_names$med2_rxnorm_p== "lisdexamfetamine"|medrx_names$med2_rxnorm_p== "methylphenidate"] <- 2

medrx_names$stimulants [medrx_names$med3_rxnorm_p== "amphetamine"|medrx_names$med3_rxnorm_p== "caffeine"|
                             medrx_names$med3_rxnorm_p== "dexmethylphenidate"|medrx_names$med3_rxnorm_p== "dextroamphetamine"|
                             medrx_names$med3_rxnorm_p== "lisdexamfetamine"|medrx_names$med3_rxnorm_p== "methylphenidate"] <- 2

medrx_names$stimulants [medrx_names$med4_rxnorm_p== "amphetamine"|medrx_names$med4_rxnorm_p== "caffeine"|
                             medrx_names$med4_rxnorm_p== "dexmethylphenidate"|medrx_names$med4_rxnorm_p== "dextroamphetamine"|
                             medrx_names$med4_rxnorm_p== "lisdexamfetamine"|medrx_names$med4_rxnorm_p== "methylphenidate"] <- 2

medrx_names$stimulants [medrx_names$med5_rxnorm_p== "amphetamine"|medrx_names$med5_rxnorm_p== "caffeine"|
                             medrx_names$med5_rxnorm_p== "dexmethylphenidate"|medrx_names$med5_rxnorm_p== "dextroamphetamine"|
                             medrx_names$med5_rxnorm_p== "lisdexamfetamine"|medrx_names$med5_rxnorm_p== "methylphenidate"] <- 2

medrx_names$stimulants [medrx_names$med6_rxnorm_p== "amphetamine"|medrx_names$med6_rxnorm_p== "caffeine"|
                          medrx_names$med6_rxnorm_p== "dexmethylphenidate"|medrx_names$med6_rxnorm_p== "dextroamphetamine"|
                          medrx_names$med6_rxnorm_p== "lisdexamfetamine"|medrx_names$med6_rxnorm_p== "methylphenidate"] <- 2

medrx_names$stimulants [medrx_names$med7_rxnorm_p== "amphetamine"|medrx_names$med7_rxnorm_p== "caffeine"|
                          medrx_names$med7_rxnorm_p== "dexmethylphenidate"|medrx_names$med7_rxnorm_p== "dextroamphetamine"|
                          medrx_names$med7_rxnorm_p== "lisdexamfetamine"|medrx_names$med7_rxnorm_p== "methylphenidate"] <- 2

medrx_names$stimulants [medrx_names$med8_rxnorm_p== "amphetamine"|medrx_names$med8_rxnorm_p== "caffeine"|
                          medrx_names$med8_rxnorm_p== "dexmethylphenidate"|medrx_names$med8_rxnorm_p== "dextroamphetamine"|
                          medrx_names$med8_rxnorm_p== "lisdexamfetamine"|medrx_names$med8_rxnorm_p== "methylphenidate"] <- 2

medrx_names$stimulants [medrx_names$med9_rxnorm_p== "amphetamine"|medrx_names$med9_rxnorm_p== "caffeine"|
                          medrx_names$med9_rxnorm_p== "dexmethylphenidate"|medrx_names$med9_rxnorm_p== "dextroamphetamine"|
                          medrx_names$med9_rxnorm_p== "lisdexamfetamine"|medrx_names$med9_rxnorm_p== "methylphenidate"] <- 2

medrx_names$stimulants [medrx_names$med10_rxnorm_p== "amphetamine"|medrx_names$med10_rxnorm_p== "caffeine"|
                          medrx_names$med10_rxnorm_p== "dexmethylphenidate"|medrx_names$med10_rxnorm_p== "dextroamphetamine"|
                          medrx_names$med10_rxnorm_p== "lisdexamfetamine"|medrx_names$med10_rxnorm_p== "methylphenidate"] <- 2

medrx_names$stimulants [medrx_names$med11_rxnorm_p== "amphetamine"|medrx_names$med11_rxnorm_p== "caffeine"|
                          medrx_names$med11_rxnorm_p== "dexmethylphenidate"|medrx_names$med11_rxnorm_p== "dextroamphetamine"|
                          medrx_names$med11_rxnorm_p== "lisdexamfetamine"|medrx_names$med11_rxnorm_p== "methylphenidate"] <- 2

medrx_names$stimulants [medrx_names$med12_rxnorm_p== "amphetamine"|medrx_names$med12_rxnorm_p== "caffeine"|
                          medrx_names$med12_rxnorm_p== "dexmethylphenidate"|medrx_names$med12_rxnorm_p== "dextroamphetamine"|
                          medrx_names$med12_rxnorm_p== "lisdexamfetamine"|medrx_names$med12_rxnorm_p== "methylphenidate"] <- 2

medrx_names$stimulants [medrx_names$med13_rxnorm_p== "amphetamine"|medrx_names$med13_rxnorm_p== "caffeine"|
                          medrx_names$med13_rxnorm_p== "dexmethylphenidate"|medrx_names$med13_rxnorm_p== "dextroamphetamine"|
                          medrx_names$med13_rxnorm_p== "lisdexamfetamine"|medrx_names$med13_rxnorm_p== "methylphenidate"] <- 2

medrx_names$stimulants [medrx_names$med14_rxnorm_p== "amphetamine"|medrx_names$med14_rxnorm_p== "caffeine"|
                          medrx_names$med14_rxnorm_p== "dexmethylphenidate"|medrx_names$med14_rxnorm_p== "dextroamphetamine"|
                          medrx_names$med14_rxnorm_p== "lisdexamfetamine"|medrx_names$med14_rxnorm_p== "methylphenidate"] <- 2

medrx_names$stimulants [medrx_names$med15_rxnorm_p== "amphetamine"|medrx_names$med15_rxnorm_p== "caffeine"|
                          medrx_names$med15_rxnorm_p== "dexmethylphenidate"|medrx_names$med15_rxnorm_p== "dextroamphetamine"|
                          medrx_names$med15_rxnorm_p== "lisdexamfetamine"|medrx_names$med15_rxnorm_p== "methylphenidate"] <- 2


sum(!is.na(medrx_names$stimulants))

#analgesics = 5
medrx_names$analgesics <- NA

medrx_names$analgesics [medrx_names$med1_rxnorm_p== "acetaminophen"|medrx_names$med1_rxnorm_p== "acetylsalicylic acid"|
                             medrx_names$med1_rxnorm_p== "amitriptyline"|medrx_names$med1_rxnorm_p== "amitriptyline hydrochloride"|
                             medrx_names$med1_rxnorm_p== "hydrocodone"|medrx_names$med1_rxnorm_p== "hydrocodone-chlorpheniramine"|
                             medrx_names$med1_rxnorm_p== "ibuprofen"|medrx_names$med1_rxnorm_p== "meloxicam"| 
                             medrx_names$med1_rxnorm_p== "naproxen"|
                             medrx_names$med1_rxnorm_p== "oxaprozin"|medrx_names$med1_rxnorm_p== "oxycodone"|
                             medrx_names$med1_rxnorm_p== "pimecrolimus"|medrx_names$med1_rxnorm_p== "sulfasalazine"] <- 5

medrx_names$analgesics [medrx_names$med2_rxnorm_p== "acetaminophen"|medrx_names$med2_rxnorm_p== "acetylsalicylic acid"|
                             medrx_names$med2_rxnorm_p== "amitriptyline"|medrx_names$med2_rxnorm_p== "amitriptyline hydrochloride"|
                             medrx_names$med2_rxnorm_p== "hydrocodone"|medrx_names$med2_rxnorm_p== "hydrocodone-chlorpheniramine"|
                             medrx_names$med2_rxnorm_p== "ibuprofen"|medrx_names$med2_rxnorm_p== "meloxicam"|
                             medrx_names$med2_rxnorm_p== "naproxen"|
                             medrx_names$med2_rxnorm_p== "oxaprozin"|medrx_names$med2_rxnorm_p== "oxycodone"|
                             medrx_names$med2_rxnorm_p== "pimecrolimus"|medrx_names$med2_rxnorm_p== "sulfasalazine"] <- 5

medrx_names$analgesics [medrx_names$med3_rxnorm_p== "acetaminophen"|medrx_names$med3_rxnorm_p== "acetylsalicylic acid"|
                             medrx_names$med3_rxnorm_p== "amitriptyline"|medrx_names$med3_rxnorm_p== "amitriptyline hydrochloride"|
                             medrx_names$med3_rxnorm_p== "hydrocodone"|medrx_names$med3_rxnorm_p== "hydrocodone-chlorpheniramine"|
                             medrx_names$med3_rxnorm_p== "ibuprofen"|medrx_names$med3_rxnorm_p== "meloxicam"|
                             medrx_names$med3_rxnorm_p== "naproxen"|
                             medrx_names$med3_rxnorm_p== "oxaprozin"|medrx_names$med3_rxnorm_p== "oxycodone"|
                             medrx_names$med3_rxnorm_p== "pimecrolimus"|medrx_names$med3_rxnorm_p== "sulfasalazine"] <- 5

medrx_names$analgesics [medrx_names$med4_rxnorm_p== "acetaminophen"|medrx_names$med4_rxnorm_p== "acetylsalicylic acid"|
                             medrx_names$med4_rxnorm_p== "amitriptyline"|medrx_names$med4_rxnorm_p== "amitriptyline hydrochloride"|
                             medrx_names$med4_rxnorm_p== "hydrocodone"|medrx_names$med4_rxnorm_p== "hydrocodone-chlorpheniramine"|
                             medrx_names$med4_rxnorm_p== "ibuprofen"|medrx_names$med4_rxnorm_p== "meloxicam"|
                             medrx_names$med4_rxnorm_p== "naproxen"|
                             medrx_names$med4_rxnorm_p== "oxaprozin"|medrx_names$med4_rxnorm_p== "oxycodone"|
                             medrx_names$med4_rxnorm_p== "pimecrolimus"|medrx_names$med4_rxnorm_p== "sulfasalazine"] <- 5

medrx_names$analgesics [medrx_names$med5_rxnorm_p== "acetaminophen"|medrx_names$med5_rxnorm_p== "acetylsalicylic acid"|
                             medrx_names$med5_rxnorm_p== "amitriptyline"|medrx_names$med5_rxnorm_p== "amitriptyline hydrochloride"|
                             medrx_names$med5_rxnorm_p== "hydrocodone"|medrx_names$med5_rxnorm_p== "hydrocodone-chlorpheniramine"|
                             medrx_names$med5_rxnorm_p== "ibuprofen"|medrx_names$med5_rxnorm_p== "meloxicam"|
                             medrx_names$med5_rxnorm_p== "naproxen"|
                             medrx_names$med5_rxnorm_p== "oxaprozin"|medrx_names$med5_rxnorm_p== "oxycodone"|
                             medrx_names$med5_rxnorm_p== "pimecrolimus"|medrx_names$med5_rxnorm_p== "sulfasalazine"] <- 5

medrx_names$analgesics [medrx_names$med13_rxnorm_p== "ibuprofen"] <- 5

sum(!is.na(medrx_names$analgesics))

#inhalants = 6
medrx_names$inhalants <- NA

medrx_names$inhalants [medrx_names$med1_rxnorm_p== "albuterol"|medrx_names$med1_rxnorm_p== "ambroxol clenbuterol"|
                             medrx_names$med1_rxnorm_p== "azelastine-fluticasone"|medrx_names$med1_rxnorm_p== "beclomethasone"|
                             medrx_names$med1_rxnorm_p== "camphor-eucalyptus oil-menthol"|medrx_names$med1_rxnorm_p== "cromolyn sodium"|
                             medrx_names$med1_rxnorm_p== "dornase"|medrx_names$med1_rxnorm_p== "dudesonide-formoterol"|
                             medrx_names$med1_rxnorm_p== "fluticasone"|medrx_names$med1_rxnorm_p== "formoterol"|
                             medrx_names$med1_rxnorm_p== "ipratropium"|medrx_names$med1_rxnorm_p== "ipratropium bromide monohydrate"] <- 6

medrx_names$inhalants [medrx_names$med2_rxnorm_p== "albuterol"|medrx_names$med2_rxnorm_p== "ambroxol clenbuterol"|
                             medrx_names$med2_rxnorm_p== "azelastine-fluticasone"|medrx_names$med2_rxnorm_p== "beclomethasone"|
                             medrx_names$med2_rxnorm_p== "camphor-eucalyptus oil-menthol"|medrx_names$med2_rxnorm_p== "cromolyn sodium"|
                             medrx_names$med2_rxnorm_p== "dornase"|medrx_names$med2_rxnorm_p== "dudesonide-formoterol"|
                             medrx_names$med2_rxnorm_p== "fluticasone"|medrx_names$med2_rxnorm_p== "formoterol"|
                             medrx_names$med2_rxnorm_p== "ipratropium"|medrx_names$med2_rxnorm_p== "ipratropium bromide monohydrate"] <- 6

medrx_names$inhalants [medrx_names$med3_rxnorm_p== "albuterol"|medrx_names$med3_rxnorm_p== "ambroxol clenbuterol"|
                             medrx_names$med3_rxnorm_p== "azelastine-fluticasone"|medrx_names$med3_rxnorm_p== "beclomethasone"|
                             medrx_names$med3_rxnorm_p== "camphor-eucalyptus oil-menthol"|medrx_names$med3_rxnorm_p== "cromolyn sodium"|
                             medrx_names$med3_rxnorm_p== "dornase"|medrx_names$med3_rxnorm_p== "dudesonide-formoterol"|
                             medrx_names$med3_rxnorm_p== "fluticasone"|medrx_names$med3_rxnorm_p== "formoterol"|
                             medrx_names$med3_rxnorm_p== "ipratropium"|medrx_names$med3_rxnorm_p== "ipratropium bromide monohydrate"] <- 6

medrx_names$inhalants [medrx_names$med4_rxnorm_p== "albuterol"|medrx_names$med4_rxnorm_p== "ambroxol clenbuterol"|
                             medrx_names$med4_rxnorm_p== "azelastine-fluticasone"|medrx_names$med4_rxnorm_p== "beclomethasone"|
                             medrx_names$med4_rxnorm_p== "camphor-eucalyptus oil-menthol"|medrx_names$med4_rxnorm_p== "cromolyn sodium"|
                             medrx_names$med4_rxnorm_p== "dornase"|medrx_names$med4_rxnorm_p== "dudesonide-formoterol"|
                             medrx_names$med4_rxnorm_p== "fluticasone"|medrx_names$med4_rxnorm_p== "formoterol"|
                             medrx_names$med4_rxnorm_p== "ipratropium"|medrx_names$med4_rxnorm_p== "ipratropium bromide monohydrate"] <- 6

medrx_names$inhalants [medrx_names$med5_rxnorm_p== "albuterol"|medrx_names$med5_rxnorm_p== "ambroxol clenbuterol"|
                             medrx_names$med5_rxnorm_p== "azelastine-fluticasone"|medrx_names$med5_rxnorm_p== "beclomethasone"|
                             medrx_names$med5_rxnorm_p== "camphor-eucalyptus oil-menthol"|medrx_names$med5_rxnorm_p== "cromolyn sodium"|
                             medrx_names$med5_rxnorm_p== "dornase"|medrx_names$med5_rxnorm_p== "dudesonide-formoterol"|
                             medrx_names$med5_rxnorm_p== "fluticasone"|medrx_names$med5_rxnorm_p== "formoterol"|
                             medrx_names$med5_rxnorm_p== "ipratropium"|medrx_names$med5_rxnorm_p== "ipratropium bromide monohydrate"] <- 6

medrx_names$inhalants [medrx_names$med6_rxnorm_p== "albuterol"|medrx_names$med6_rxnorm_p== "ambroxol clenbuterol"|
                             medrx_names$med6_rxnorm_p== "azelastine-fluticasone"|medrx_names$med6_rxnorm_p== "beclomethasone"|
                             medrx_names$med6_rxnorm_p== "camphor-eucalyptus oil-menthol"|medrx_names$med6_rxnorm_p== "cromolyn sodium"|
                             medrx_names$med6_rxnorm_p== "dornase"|medrx_names$med6_rxnorm_p== "dudesonide-formoterol"|
                             medrx_names$med6_rxnorm_p== "fluticasone"|medrx_names$med6_rxnorm_p== "formoterol"|
                             medrx_names$med6_rxnorm_p== "ipratropium"|medrx_names$med6_rxnorm_p== "ipratropium bromide monohydrate"] <- 6

medrx_names$inhalants [medrx_names$med7_rxnorm_p== "albuterol"|medrx_names$med7_rxnorm_p== "beclomethasone"|
                             medrx_names$med7_rxnorm_p== "fluticasone"] <- 6

medrx_names$inhalants [medrx_names$med8_rxnorm_p== "albuterol"| medrx_names$med8_rxnorm_p== "fluticasone"] <- 6

medrx_names$inhalants [medrx_names$med9_rxnorm_p== "albuterol"| medrx_names$med9_rxnorm_p== "fluticasone"] <- 6

medrx_names$inhalants [medrx_names$med10_rxnorm_p== "albuterol"| medrx_names$med10_rxnorm_p== "fluticasone"] <- 6

medrx_names$inhalants [medrx_names$med12_rxnorm_p== "albuterol"|medrx_names$med12_rxnorm_p== "beclomethasone"] <- 6

medrx_names$inhalants [medrx_names$med13_rxnorm_p== "albuterol"] <- 6

sum(!is.na(medrx_names$inhalants))

#cannabis = 7
medrx_names$cannabis <- NA

medrx_names$cannabis [medrx_names$med1_rxnorm_p== "cannabinol"] <- 7

sum(!is.na(medrx_names$cannabis))

#antibiotics = 9
medrx_names$antibiotics <- NA

medrx_names$antibiotics [medrx_names$med1_rxnorm_p== "albendazole"| medrx_names$med1_rxnorm_p== "amoxicillin"| 
                             medrx_names$med1_rxnorm_p== "amoxicillin/clavulanate"| medrx_names$med1_rxnorm_p== "ampicillin" |
                             medrx_names$med1_rxnorm_p== "azithromycin"|medrx_names$med1_rxnorm_p== "cefdinir"|
                             medrx_names$med1_rxnorm_p== "cefuroxime"|medrx_names$med1_rxnorm_p== "cephalexin"|
                             medrx_names$med1_rxnorm_p== "ciclesonide"|medrx_names$med1_rxnorm_p== "ciprofloxacin"|
                             medrx_names$med1_rxnorm_p== "ciprofloxacin/dexamethasone"|medrx_names$med1_rxnorm_p== "clindamycin"|
                             medrx_names$med1_rxnorm_p== "doxycycline"|medrx_names$med1_rxnorm_p== "erythromycin"|
                             medrx_names$med1_rxnorm_p== "gentamicin sulfate"|medrx_names$med1_rxnorm_p== "metronidazole"|
                             medrx_names$med1_rxnorm_p== "mupirocin"|medrx_names$med1_rxnorm_p== "neomycin-bacitracnzn-polymyxnb"|
                             medrx_names$med1_rxnorm_p== "nitrofurantoin"|medrx_names$med1_rxnorm_p== "penicillin"|
                             medrx_names$med1_rxnorm_p== "polymyxin b"|medrx_names$med1_rxnorm_p== "sulfadiazine"|
                             medrx_names$med1_rxnorm_p== "sulfamethoxazole"|medrx_names$med1_rxnorm_p== "sulfamethoxazole trimethoprim"|
                             medrx_names$med1_rxnorm_p== "tobramycin"] <- 9

medrx_names$antibiotics [medrx_names$med2_rxnorm_p== "albendazole"| medrx_names$med2_rxnorm_p== "amoxicillin"| 
                             medrx_names$med2_rxnorm_p== "amoxicillin/clavulanate"| medrx_names$med2_rxnorm_p== "ampicillin" |
                             medrx_names$med2_rxnorm_p== "azithromycin"|medrx_names$med2_rxnorm_p== "cefdinir"|
                             medrx_names$med2_rxnorm_p== "cefuroxime"|medrx_names$med2_rxnorm_p== "cephalexin"|
                             medrx_names$med2_rxnorm_p== "ciclesonide"|medrx_names$med2_rxnorm_p== "ciprofloxacin"|
                             medrx_names$med2_rxnorm_p== "ciprofloxacin/dexamethasone"|medrx_names$med2_rxnorm_p== "clindamycin"|
                             medrx_names$med2_rxnorm_p== "doxycycline"|medrx_names$med2_rxnorm_p== "erythromycin"|
                             medrx_names$med2_rxnorm_p== "gentamicin sulfate"|medrx_names$med2_rxnorm_p== "metronidazole"|
                             medrx_names$med2_rxnorm_p== "mupirocin"|medrx_names$med2_rxnorm_p== "neomycin-bacitracnzn-polymyxnb"|
                             medrx_names$med2_rxnorm_p== "nitrofurantoin"|medrx_names$med2_rxnorm_p== "penicillin"|
                             medrx_names$med2_rxnorm_p== "polymyxin b"|medrx_names$med2_rxnorm_p== "sulfadiazine"|
                             medrx_names$med2_rxnorm_p== "sulfamethoxazole"|medrx_names$med2_rxnorm_p== "sulfamethoxazole trimethoprim"|
                             medrx_names$med2_rxnorm_p== "tobramycin"] <- 9

medrx_names$antibiotics [medrx_names$med3_rxnorm_p== "albendazole"| medrx_names$med3_rxnorm_p== "amoxicillin"| 
                             medrx_names$med3_rxnorm_p== "amoxicillin/clavulanate"| medrx_names$med3_rxnorm_p== "ampicillin" |
                             medrx_names$med3_rxnorm_p== "azithromycin"|medrx_names$med3_rxnorm_p== "cefdinir"|
                             medrx_names$med3_rxnorm_p== "cefuroxime"|medrx_names$med3_rxnorm_p== "cephalexin"|
                             medrx_names$med3_rxnorm_p== "ciclesonide"|medrx_names$med3_rxnorm_p== "ciprofloxacin"|
                             medrx_names$med3_rxnorm_p== "ciprofloxacin/dexamethasone"|medrx_names$med3_rxnorm_p== "clindamycin"|
                             medrx_names$med3_rxnorm_p== "doxycycline"|medrx_names$med3_rxnorm_p== "erythromycin"|
                             medrx_names$med3_rxnorm_p== "gentamicin sulfate"|medrx_names$med3_rxnorm_p== "metronidazole"|
                             medrx_names$med3_rxnorm_p== "mupirocin"|medrx_names$med3_rxnorm_p== "neomycin-bacitracnzn-polymyxnb"|
                             medrx_names$med3_rxnorm_p== "nitrofurantoin"|medrx_names$med3_rxnorm_p== "penicillin"|
                             medrx_names$med3_rxnorm_p== "polymyxin b"|medrx_names$med3_rxnorm_p== "sulfadiazine"|
                             medrx_names$med3_rxnorm_p== "sulfamethoxazole"|medrx_names$med3_rxnorm_p== "sulfamethoxazole trimethoprim"|
                             medrx_names$med3_rxnorm_p== "tobramycin"] <- 9

medrx_names$antibiotics [medrx_names$med4_rxnorm_p== "albendazole"| medrx_names$med4_rxnorm_p== "amoxicillin"| 
                             medrx_names$med4_rxnorm_p== "amoxicillin/clavulanate"| medrx_names$med4_rxnorm_p== "ampicillin" |
                             medrx_names$med4_rxnorm_p== "azithromycin"|medrx_names$med4_rxnorm_p== "cefdinir"|
                             medrx_names$med4_rxnorm_p== "cefuroxime"|medrx_names$med4_rxnorm_p== "cephalexin"|
                             medrx_names$med4_rxnorm_p== "ciclesonide"|medrx_names$med4_rxnorm_p== "ciprofloxacin"|
                             medrx_names$med4_rxnorm_p== "ciprofloxacin/dexamethasone"|medrx_names$med4_rxnorm_p== "clindamycin"|
                             medrx_names$med4_rxnorm_p== "doxycycline"|medrx_names$med4_rxnorm_p== "erythromycin"|
                             medrx_names$med4_rxnorm_p== "gentamicin sulfate"|medrx_names$med4_rxnorm_p== "metronidazole"|
                             medrx_names$med4_rxnorm_p== "mupirocin"|medrx_names$med4_rxnorm_p== "neomycin-bacitracnzn-polymyxnb"|
                             medrx_names$med4_rxnorm_p== "nitrofurantoin"|medrx_names$med4_rxnorm_p== "penicillin"|
                             medrx_names$med4_rxnorm_p== "polymyxin b"|medrx_names$med4_rxnorm_p== "sulfadiazine"|
                             medrx_names$med4_rxnorm_p== "sulfamethoxazole"|medrx_names$med4_rxnorm_p== "sulfamethoxazole trimethoprim"|
                             medrx_names$med4_rxnorm_p== "tobramycin"] <- 9

medrx_names$antibiotics [medrx_names$med5_rxnorm_p== "albendazole"| medrx_names$med5_rxnorm_p== "amoxicillin"| 
                             medrx_names$med5_rxnorm_p== "amoxicillin/clavulanate"| medrx_names$med5_rxnorm_p== "ampicillin" |
                             medrx_names$med5_rxnorm_p== "azithromycin"|medrx_names$med5_rxnorm_p== "cefdinir"|
                             medrx_names$med5_rxnorm_p== "cefuroxime"|medrx_names$med5_rxnorm_p== "cephalexin"|
                             medrx_names$med5_rxnorm_p== "ciclesonide"|medrx_names$med5_rxnorm_p== "ciprofloxacin"|
                             medrx_names$med5_rxnorm_p== "ciprofloxacin/dexamethasone"|medrx_names$med5_rxnorm_p== "clindamycin"|
                             medrx_names$med5_rxnorm_p== "doxycycline"|medrx_names$med5_rxnorm_p== "erythromycin"|
                             medrx_names$med5_rxnorm_p== "gentamicin sulfate"|medrx_names$med5_rxnorm_p== "metronidazole"|
                             medrx_names$med5_rxnorm_p== "mupirocin"|medrx_names$med5_rxnorm_p== "neomycin-bacitracnzn-polymyxnb"|
                             medrx_names$med5_rxnorm_p== "nitrofurantoin"|medrx_names$med5_rxnorm_p== "penicillin"|
                             medrx_names$med5_rxnorm_p== "polymyxin b"|medrx_names$med5_rxnorm_p== "sulfadiazine"|
                             medrx_names$med5_rxnorm_p== "sulfamethoxazole"|medrx_names$med5_rxnorm_p== "sulfamethoxazole trimethoprim"|
                             medrx_names$med5_rxnorm_p== "tobramycin"] <- 9

medrx_names$antibiotics [medrx_names$med6_rxnorm_p== "albendazole"| medrx_names$med6_rxnorm_p== "amoxicillin"| 
                             medrx_names$med6_rxnorm_p== "amoxicillin/clavulanate"| medrx_names$med6_rxnorm_p== "ampicillin" |
                             medrx_names$med6_rxnorm_p== "azithromycin"|medrx_names$med6_rxnorm_p== "cefdinir"|
                             medrx_names$med6_rxnorm_p== "cefuroxime"|medrx_names$med6_rxnorm_p== "cephalexin"|
                             medrx_names$med6_rxnorm_p== "ciclesonide"|medrx_names$med6_rxnorm_p== "ciprofloxacin"|
                             medrx_names$med6_rxnorm_p== "ciprofloxacin/dexamethasone"|medrx_names$med6_rxnorm_p== "clindamycin"|
                             medrx_names$med6_rxnorm_p== "doxycycline"|medrx_names$med6_rxnorm_p== "erythromycin"|
                             medrx_names$med6_rxnorm_p== "gentamicin sulfate"|medrx_names$med6_rxnorm_p== "metronidazole"|
                             medrx_names$med6_rxnorm_p== "mupirocin"|medrx_names$med6_rxnorm_p== "neomycin-bacitracnzn-polymyxnb"|
                             medrx_names$med6_rxnorm_p== "nitrofurantoin"|medrx_names$med6_rxnorm_p== "penicillin"|
                             medrx_names$med6_rxnorm_p== "polymyxin b"|medrx_names$med6_rxnorm_p== "sulfadiazine"|
                             medrx_names$med6_rxnorm_p== "sulfamethoxazole"|medrx_names$med6_rxnorm_p== "sulfamethoxazole trimethoprim"|
                             medrx_names$med6_rxnorm_p== "tobramycin"] <- 9

medrx_names$antibiotics [medrx_names$med9_rxnorm_p== "sulfamethoxazole trimethoprim"] <- 9

medrx_names$antibiotics [medrx_names$med10_rxnorm_p== "mupirocin"] <- 9

medrx_names$antibiotics [medrx_names$med11_rxnorm_p== "tobramycin"] <- 9

sum(!is.na(medrx_names$antibiotics))

#adrenergic agent = 10
medrx_names$adrenergic_agent <- NA

medrx_names$adrenergic_agent [medrx_names$med1_rxnorm_p== "bisoprolol fumarate"|medrx_names$med1_rxnorm_p== "doxazosin mesylate"|
                             medrx_names$med1_rxnorm_p== "epinephrine"|medrx_names$med1_rxnorm_p== "guanabenz"|
                             medrx_names$med1_rxnorm_p== "guanfacine"|medrx_names$med1_rxnorm_p== "imipramine"|
                             medrx_names$med1_rxnorm_p== "levalbuterol"|medrx_names$med1_rxnorm_p== "metoprolol succinate"|
                             medrx_names$med1_rxnorm_p== "nadolol"|medrx_names$med1_rxnorm_p== "oxymetazoline"|
                             medrx_names$med1_rxnorm_p== "pseudoephedrine"] <- 10

medrx_names$adrenergic_agent [medrx_names$med2_rxnorm_p== "bisoprolol fumarate"|medrx_names$med2_rxnorm_p== "doxazosin mesylate"|
                             medrx_names$med2_rxnorm_p== "epinephrine"|medrx_names$med2_rxnorm_p== "guanabenz"|
                             medrx_names$med2_rxnorm_p== "guanfacine"|medrx_names$med2_rxnorm_p== "imipramine"|
                             medrx_names$med2_rxnorm_p== "levalbuterol"|medrx_names$med2_rxnorm_p== "metoprolol succinate"|
                             medrx_names$med2_rxnorm_p== "nadolol"|medrx_names$med2_rxnorm_p== "oxymetazoline"|
                             medrx_names$med2_rxnorm_p== "pseudoephedrine"] <- 10

medrx_names$adrenergic_agent [medrx_names$med3_rxnorm_p== "bisoprolol fumarate"|medrx_names$med3_rxnorm_p== "doxazosin mesylate"|
                             medrx_names$med3_rxnorm_p== "epinephrine"|medrx_names$med3_rxnorm_p== "guanabenz"|
                             medrx_names$med3_rxnorm_p== "guanfacine"|medrx_names$med3_rxnorm_p== "imipramine"|
                             medrx_names$med3_rxnorm_p== "levalbuterol"|medrx_names$med3_rxnorm_p== "metoprolol succinate"|
                             medrx_names$med3_rxnorm_p== "nadolol"|medrx_names$med3_rxnorm_p== "oxymetazoline"|
                             medrx_names$med3_rxnorm_p== "pseudoephedrine"] <- 10

medrx_names$adrenergic_agent [medrx_names$med4_rxnorm_p== "bisoprolol fumarate"|medrx_names$med4_rxnorm_p== "doxazosin mesylate"|
                             medrx_names$med4_rxnorm_p== "epinephrine"|medrx_names$med4_rxnorm_p== "guanabenz"|
                             medrx_names$med4_rxnorm_p== "guanfacine"|medrx_names$med4_rxnorm_p== "imipramine"|
                             medrx_names$med4_rxnorm_p== "levalbuterol"|medrx_names$med4_rxnorm_p== "metoprolol succinate"|
                             medrx_names$med4_rxnorm_p== "nadolol"|medrx_names$med4_rxnorm_p== "oxymetazoline"|
                             medrx_names$med4_rxnorm_p== "pseudoephedrine"] <- 10

medrx_names$adrenergic_agent [medrx_names$med5_rxnorm_p== "bisoprolol fumarate"|medrx_names$med5_rxnorm_p== "doxazosin mesylate"|
                             medrx_names$med5_rxnorm_p== "epinephrine"|medrx_names$med5_rxnorm_p== "guanabenz"|
                             medrx_names$med5_rxnorm_p== "guanfacine"|medrx_names$med5_rxnorm_p== "imipramine"|
                             medrx_names$med5_rxnorm_p== "levalbuterol"|medrx_names$med5_rxnorm_p== "metoprolol succinate"|
                             medrx_names$med5_rxnorm_p== "nadolol"|medrx_names$med5_rxnorm_p== "oxymetazoline"|
                             medrx_names$med5_rxnorm_p== "pseudoephedrine"] <- 10

medrx_names$adrenergic_agent [ medrx_names$med6_rxnorm_p== "guanfacine"] <- 10

medrx_names$adrenergic_agent [ medrx_names$med11_rxnorm_p== "levalbuterol"] <- 10

medrx_names$adrenergic_agent [ medrx_names$med12_rxnorm_p== "levalbuterol"] <- 10

#antihistamine = 11
medrx_names$antihistamine <- NA

medrx_names$antihistamine [medrx_names$med1_rxnorm_p== "azelastine"|medrx_names$med1_rxnorm_p== "brompheniramine"|
                             medrx_names$med1_rxnorm_p== "cetirizine"|medrx_names$med1_rxnorm_p== "chlorpheniramine"|
                             medrx_names$med1_rxnorm_p== "cyproheptadine"|medrx_names$med1_rxnorm_p== "desloratadine"|
                             medrx_names$med1_rxnorm_p== "diphenhydramine"|medrx_names$med1_rxnorm_p== "fexofenadine"|
                             medrx_names$med1_rxnorm_p== "ketotifen"|medrx_names$med1_rxnorm_p== "loratadine"|
                             medrx_names$med1_rxnorm_p== "olopatadine"| medrx_names$med1_rxnorm_p== "periactin"] <- 11

medrx_names$antihistamine [medrx_names$med2_rxnorm_p== "azelastine"|medrx_names$med2_rxnorm_p== "brompheniramine"|
                             medrx_names$med2_rxnorm_p== "cetirizine"|medrx_names$med2_rxnorm_p== "chlorpheniramine"|
                             medrx_names$med2_rxnorm_p== "cyproheptadine"|medrx_names$med2_rxnorm_p== "desloratadine"|
                             medrx_names$med2_rxnorm_p== "diphenhydramine"|medrx_names$med2_rxnorm_p== "fexofenadine"|
                             medrx_names$med2_rxnorm_p== "ketotifen"|medrx_names$med2_rxnorm_p== "loratadine"|
                             medrx_names$med2_rxnorm_p== "olopatadine"| medrx_names$med2_rxnorm_p== "periactin"] <- 11

medrx_names$antihistamine [medrx_names$med3_rxnorm_p== "azelastine"|medrx_names$med3_rxnorm_p== "brompheniramine"|
                             medrx_names$med3_rxnorm_p== "cetirizine"|medrx_names$med3_rxnorm_p== "chlorpheniramine"|
                             medrx_names$med3_rxnorm_p== "cyproheptadine"|medrx_names$med3_rxnorm_p== "desloratadine"|
                             medrx_names$med3_rxnorm_p== "diphenhydramine"|medrx_names$med3_rxnorm_p== "fexofenadine"|
                             medrx_names$med3_rxnorm_p== "ketotifen"|medrx_names$med3_rxnorm_p== "loratadine"|
                             medrx_names$med3_rxnorm_p== "olopatadine"| medrx_names$med3_rxnorm_p== "periactin"] <- 11

medrx_names$antihistamine [medrx_names$med4_rxnorm_p== "azelastine"|medrx_names$med4_rxnorm_p== "brompheniramine"|
                             medrx_names$med4_rxnorm_p== "cetirizine"|medrx_names$med4_rxnorm_p== "chlorpheniramine"|
                             medrx_names$med4_rxnorm_p== "cyproheptadine"|medrx_names$med4_rxnorm_p== "desloratadine"|
                             medrx_names$med4_rxnorm_p== "diphenhydramine"|medrx_names$med4_rxnorm_p== "fexofenadine"|
                             medrx_names$med4_rxnorm_p== "ketotifen"|medrx_names$med4_rxnorm_p== "loratadine"|
                             medrx_names$med4_rxnorm_p== "olopatadine"| medrx_names$med4_rxnorm_p== "periactin"] <- 11

medrx_names$antihistamine [medrx_names$med5_rxnorm_p== "azelastine"|medrx_names$med5_rxnorm_p== "brompheniramine"|
                             medrx_names$med5_rxnorm_p== "cetirizine"|medrx_names$med5_rxnorm_p== "chlorpheniramine"|
                             medrx_names$med5_rxnorm_p== "cyproheptadine"|medrx_names$med5_rxnorm_p== "desloratadine"|
                             medrx_names$med5_rxnorm_p== "diphenhydramine"|medrx_names$med5_rxnorm_p== "fexofenadine"|
                             medrx_names$med5_rxnorm_p== "ketotifen"|medrx_names$med5_rxnorm_p== "loratadine"|
                             medrx_names$med5_rxnorm_p== "olopatadine"| medrx_names$med5_rxnorm_p== "periactin"] <- 11

medrx_names$antihistamine [medrx_names$med6_rxnorm_p== "cetirizine"|medrx_names$med6_rxnorm_p== "diphenhydramine"] <- 11

medrx_names$antihistamine [medrx_names$med7_rxnorm_p== "cetirizine"] <- 11

medrx_names$antihistamine [medrx_names$med8_rxnorm_p== "cetirizine"|medrx_names$med8_rxnorm_p== "desloratadine"] <- 11

medrx_names$antihistamine [medrx_names$med5_rxnorm_p== "cetirizine"|medrx_names$med10_rxnorm_p== "olopatadine"] <- 11

medrx_names$antihistamine [medrx_names$med12_rxnorm_p== "olopatadine"] <- 11

medrx_names$antihistamine [medrx_names$med14_rxnorm_p== "diphenhydramine"] <- 11

#corticosteroid = 12
medrx_names$corticosteroid <- NA

medrx_names$corticosteroid [medrx_names$med1_rxnorm_p== "alclometasone"|medrx_names$med1_rxnorm_p== "asmanex"|
                             medrx_names$med1_rxnorm_p== "betamethasone"|medrx_names$med1_rxnorm_p== "budesonide"|
                             medrx_names$med1_rxnorm_p== "clobetasol"|medrx_names$med1_rxnorm_p== "cortisone"|
                             medrx_names$med1_rxnorm_p== "desonide"|medrx_names$med1_rxnorm_p== "dexamethasone"|
                             medrx_names$med1_rxnorm_p== "fludrocortisone"|medrx_names$med1_rxnorm_p== "flunisolide"|
                             medrx_names$med1_rxnorm_p== "fluocinolone"| medrx_names$med1_rxnorm_p== "fluocinolone acetonide"|
                             medrx_names$med1_rxnorm_p== "fluocinonide"|medrx_names$med1_rxnorm_p== "hydrocortisone"|
                             medrx_names$med1_rxnorm_p== "loteprednol etabonate"|medrx_names$med1_rxnorm_p== "prednisolone"|
                             medrx_names$med1_rxnorm_p== "prednisone"| medrx_names$med1_rxnorm_p== "triamcinolone"] <- 12

medrx_names$corticosteroid [medrx_names$med2_rxnorm_p== "alclometasone"|medrx_names$med2_rxnorm_p== "asmanex"|
                             medrx_names$med2_rxnorm_p== "betamethasone"|medrx_names$med2_rxnorm_p== "budesonide"|
                             medrx_names$med2_rxnorm_p== "clobetasol"|medrx_names$med2_rxnorm_p== "cortisone"|
                             medrx_names$med2_rxnorm_p== "desonide"|medrx_names$med2_rxnorm_p== "dexamethasone"|
                             medrx_names$med2_rxnorm_p== "fludrocortisone"|medrx_names$med2_rxnorm_p== "flunisolide"|
                             medrx_names$med2_rxnorm_p== "fluocinolone"| medrx_names$med2_rxnorm_p== "fluocinolone acetonide"|
                             medrx_names$med_rxnorm_p== "fluocinonide"|medrx_names$med2_rxnorm_p== "hydrocortisone"|
                             medrx_names$med2_rxnorm_p== "loteprednol etabonate"|medrx_names$med2_rxnorm_p== "prednisolone"|
                             medrx_names$med2_rxnorm_p== "prednisone"| medrx_names$med2_rxnorm_p== "triamcinolone"] <- 12

medrx_names$corticosteroid [medrx_names$med3_rxnorm_p== "alclometasone"|medrx_names$med3_rxnorm_p== "asmanex"|
                             medrx_names$med3_rxnorm_p== "betamethasone"|medrx_names$med3_rxnorm_p== "budesonide"|
                             medrx_names$med3_rxnorm_p== "clobetasol"|medrx_names$med3_rxnorm_p== "cortisone"|
                             medrx_names$med3_rxnorm_p== "desonide"|medrx_names$med3_rxnorm_p== "dexamethasone"|
                             medrx_names$med3_rxnorm_p== "fludrocortisone"|medrx_names$med3_rxnorm_p== "flunisolide"|
                             medrx_names$med3_rxnorm_p== "fluocinolone"| medrx_names$med3_rxnorm_p== "fluocinolone acetonide"|
                             medrx_names$med3_rxnorm_p== "fluocinonide"|medrx_names$med3_rxnorm_p== "hydrocortisone"|
                             medrx_names$med3_rxnorm_p== "loteprednol etabonate"|medrx_names$med3_rxnorm_p== "prednisolone"|
                             medrx_names$med3_rxnorm_p== "prednisone"| medrx_names$med3_rxnorm_p== "triamcinolone"] <- 12

medrx_names$corticosteroid [medrx_names$med4_rxnorm_p== "alclometasone"|medrx_names$med4_rxnorm_p== "asmanex"|
                             medrx_names$med4_rxnorm_p== "betamethasone"|medrx_names$med4_rxnorm_p== "budesonide"|
                             medrx_names$med4_rxnorm_p== "clobetasol"|medrx_names$med4_rxnorm_p== "cortisone"|
                             medrx_names$med4_rxnorm_p== "desonide"|medrx_names$med4_rxnorm_p== "dexamethasone"|
                             medrx_names$med4_rxnorm_p== "fludrocortisone"|medrx_names$med4_rxnorm_p== "flunisolide"|
                             medrx_names$med4_rxnorm_p== "fluocinolone"| medrx_names$med4_rxnorm_p== "fluocinolone acetonide"|
                             medrx_names$med4_rxnorm_p== "fluocinonide"|medrx_names$med4_rxnorm_p== "hydrocortisone"|
                             medrx_names$med4_rxnorm_p== "loteprednol etabonate"|medrx_names$med4_rxnorm_p== "prednisolone"|
                             medrx_names$med4_rxnorm_p== "prednisone"| medrx_names$med4_rxnorm_p== "triamcinolone"] <- 12

medrx_names$corticosteroid [medrx_names$med5_rxnorm_p== "alclometasone"|medrx_names$med5_rxnorm_p== "asmanex"|
                             medrx_names$med5_rxnorm_p== "betamethasone"|medrx_names$med5_rxnorm_p== "budesonide"|
                             medrx_names$med5_rxnorm_p== "clobetasol"|medrx_names$med5_rxnorm_p== "cortisone"|
                             medrx_names$med5_rxnorm_p== "desonide"|medrx_names$med5_rxnorm_p== "dexamethasone"|
                             medrx_names$med5_rxnorm_p== "fludrocortisone"|medrx_names$med5_rxnorm_p== "flunisolide"|
                             medrx_names$med5_rxnorm_p== "fluocinolone"| medrx_names$med5_rxnorm_p== "fluocinolone acetonide"|
                             medrx_names$med5_rxnorm_p== "fluocinonide"|medrx_names$med5_rxnorm_p== "hydrocortisone"|
                             medrx_names$med5_rxnorm_p== "loteprednol etabonate"|medrx_names$med5_rxnorm_p== "prednisolone"|
                             medrx_names$med5_rxnorm_p== "prednisone"| medrx_names$med5_rxnorm_p== "triamcinolone"] <- 12

medrx_names$corticosteroid [medrx_names$med6_rxnorm_p== "prednisone"] <- 12

medrx_names$corticosteroid [medrx_names$med7_rxnorm_p== "hydrocortisone"] <- 12

#other = 8 
medrx_names$other <- NA

medrx_names$other [is.na(medrx_names$depressants) & is.na(medrx_names$stimulants) & is.na(medrx_names$analgesics)&
                     is.na(medrx_names$inhalants)& is.na(medrx_names$cannabis)& is.na(medrx_names$antibiotics)& 
                     is.na(medrx_names$adrenergic_agent) & is.na(medrx_names$antibiotics)& is.na(medrx_names$corticosteroid)] <- 8

#distribution
medrx_names$depressants <- as.factor(as.numeric(medrx_names$depressants))
medrx_names$stimulants <- as.factor(as.numeric(medrx_names$stimulants))
medrx_names$analgesics <- as.factor(as.numeric(medrx_names$analgesics))
medrx_names$inhalants <- as.factor(as.numeric(medrx_names$inhalants))
medrx_names$cannabis <- as.factor(as.numeric(medrx_names$cannabis))
medrx_names$antibiotics <- as.factor(as.numeric(medrx_names$antibiotics))
medrx_names$adrenergic_agent <- as.factor(as.numeric(medrx_names$adrenergic_agent))
medrx_names$antihistamine <- as.factor(as.numeric(medrx_names$antihistamine))
medrx_names$corticosteroid <- as.factor(as.numeric(medrx_names$corticosteroid))
medrx_names$other <- as.factor(as.numeric(medrx_names$other))

table1(~depressants + stimulants + analgesics + inhalants + cannabis + antibiotics + adrenergic_agent + antihistamine +
         corticosteroid + other| Group, data = medrx_names, rowlabelhead = "Drug Categories")

