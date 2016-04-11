-- |
-- Module    : Data.Kanji.Levels
-- Copyright : (c) Colin Woodbury, 2015, 2016
-- License   : GPL3
-- Maintainer: Colin Woodbury <colingw@gmail.com>
--
-- All Kanji from levels 10 to Pre-2.

module Data.Kanji.Levels where

import qualified Data.Set as S

---

-- | The Kanji unique to Level-10, studied by the end of 1st grade in
-- Japanese elementary schools.
tenth :: S.Set Char
tenth = S.fromDistinctAscList $
  "一七三上下中九二五人休先入八六円出力十千" ++
  "口右名四土夕大天女子字学小山川左年手文日" ++
  "早月木本村林校森正気水火犬玉王生田男町白" ++
  "百目石空立竹糸耳花草虫見貝赤足車金雨青音"

-- | The Kanji unique to Level-9, studied by the end of 2nd grade in
-- Japanese elementary schools.
ninth :: S.Set Char
ninth = S.fromDistinctAscList $
  "万丸交京今会体何作元兄光公内冬刀分切前北" ++
  "午半南原友古台合同回図国園地場声売夏外多" ++
  "夜太妹姉室家寺少岩工市帰広店弓引弟弱強当" ++
  "形後心思戸才教数新方明星春昼時晴曜書朝来" ++
  "東楽歌止歩母毎毛池汽活海点父牛理用画番直" ++
  "矢知社秋科答算米紙細組絵線羽考聞肉自船色" ++
  "茶行西親角言計記話語読谷買走近通週道遠里" ++
  "野長門間雪雲電頭顔風食首馬高魚鳥鳴麦黄黒"

-- | The Kanji unique to Level-8, studied by the end of 3rd grade in
-- Japanese elementary schools.
eighth :: S.Set Char
eighth = S.fromDistinctAscList $
  "丁世両主乗予事仕他代住使係倍全具写列助勉" ++
  "動勝化区医去反取受号向君味命和品員商問坂" ++
  "央始委守安定実客宮宿寒対局屋岸島州帳平幸" ++
  "度庫庭式役待急息悪悲想意感所打投拾持指放" ++
  "整旅族昔昭暑暗曲有服期板柱根植業様横橋次" ++
  "歯死氷決油波注泳洋流消深温港湖湯漢炭物球" ++
  "由申界畑病発登皮皿相県真着短研礼神祭福秒" ++
  "究章童笛第筆等箱級終緑練羊美習者育苦荷落" ++
  "葉薬血表詩調談豆負起路身転軽農返追送速進" ++
  "遊運部都配酒重鉄銀開院陽階集面題飲館駅鼻"

-- | The Kanji unique to Level-7, studied by the end of 4th grade in
-- Japanese elementary schools.
seventh :: S.Set Char
seventh = S.fromDistinctAscList $
  "不争付令以仲伝位低例便信倉候借停健側働億" ++
  "兆児共兵典冷初別利刷副功加努労勇包卒協単" ++
  "博印参史司各告周唱喜器囲固型堂塩士変夫失" ++
  "好季孫完官害察巣差希席帯底府康建径徒得必" ++
  "念愛成戦折挙改救敗散料旗昨景最望未末札材" ++
  "束松果栄案梅械極標機欠歴残殺毒氏民求治法" ++
  "泣浅浴清満漁灯無然焼照熱牧特産的省祝票種" ++
  "積競笑管節粉紀約結給続置老胃脈腸臣航良芸" ++
  "芽英菜街衣要覚観訓試説課議象貨貯費賞軍輪" ++
  "辞辺連達選郡量録鏡関陸隊静順願類飛飯養験"

-- | The Kanji unique to Level-6, studied by the end of 5th grade in
-- Japanese elementary schools.
sixth :: S.Set Char
sixth = S.fromDistinctAscList $
  "久仏仮件任似余価保修俵個備像再刊判制券則" ++
  "効務勢厚句可営因団圧在均基報境墓増夢妻婦" ++
  "容寄富導居属布師常幹序弁張往復徳志応快性" ++
  "恩情態慣承技招授採接提損支政故敵断旧易暴" ++
  "条枝査格桜検構武比永河液混減測準演潔災燃" ++
  "版犯状独率現留略益眼破確示祖禁移程税築精" ++
  "素経統絶綿総編績織罪群義耕職肥能興舌舎術" ++
  "衛製複規解設許証評講謝識護豊財貧責貸貿賀" ++
  "資賛質輸述迷退逆造過適酸鉱銅銭防限険際雑" ++
  "非預領額飼"

-- | The Kanji unique to Level-5, studied by the end of 6th grade in
-- Japanese elementary schools.
fifth :: S.Set Char
fifth = S.fromDistinctAscList $
  "並乱乳亡仁供俳値傷優党冊処刻割創劇勤危卵" ++
  "厳収后否吸呼善困垂城域奏奮姿存孝宅宇宗宙" ++
  "宝宣密寸専射将尊就尺届展層己巻幕干幼庁座" ++
  "延律従忘忠憲我批担拝拡捨探推揮操敬映晩暖" ++
  "暮朗机枚染株棒模権樹欲段沿泉洗派済源潮激" ++
  "灰熟片班異疑痛皇盛盟看砂磁私秘穀穴窓筋策" ++
  "簡糖系紅納純絹縦縮署翌聖肺背胸脳腹臓臨至" ++
  "若著蒸蔵蚕衆裁装裏補視覧討訪訳詞誌認誕誠" ++
  "誤論諸警貴賃遺郵郷針鋼閉閣降陛除障難革頂" ++
  "骨"

-- | The Kanji unique to Level-4, studied during middle school in Japan.
fourth :: S.Set Char
fourth = S.fromDistinctAscList $
  "丈与丘丹乾互井介仰伺依侵俗倒偉傍傾僧儀兼" ++
  "冒凡凶刈到刺剣剤劣勧匹占即却及叫召吐含吹" ++
  "咲唐嘆噴圏坊執堅堤塔壁壊壱奇奥奴妙姓威娘" ++
  "婚寂寝尋尽尾屈峠峰巡巨帽幅幾床弐弾彩影彼" ++
  "征御微徴忙怒怖恋恐恒恥恵悩惑惨慎慢慮憶戒" ++
  "戯扇払扱抗抜抱抵押拍拓拠振捕掘描握援搬摘" ++
  "撃攻敏敷斜旨旬是普暇暦曇更替朱朽杯枯柄柔" ++
  "桃欄歓歳殖殿汗汚沈沖沢沼況泊浜浮浸涙淡添" ++
  "渡溶滴漫澄濁濃為烈煙煮燥爆狂狩狭猛獣獲玄" ++
  "珍環甘畳疲療皆盆盗監盤盾眠瞬矛砲祈秀称稲" ++
  "稿突端箇範粒紋紫紹絡継維網緯縁繁繰罰翼耐" ++
  "肩肪胴脂脚脱腐腕腰膚致舗舞舟般芋芝茂荒菓" ++
  "蓄薄薪被襲触訴詰詳誇誉謡豪販賦贈越趣距跡" ++
  "跳踊踏躍軒較載輝輩込迎迫逃透途遅違遣避郎" ++
  "釈鈍鉛鋭鎖鑑闘陣陰隠隣隷雄雅雌離雷需震霧" ++
  "露響項頼飾香駆騒驚髪鬼鮮麗黙鼓齢"

-- | The Kanji unique to Level-3, studied by the end of middle school
-- in Japan.
third :: S.Set Char
third = S.fromDistinctAscList $
  "乏乙了企伏伐伴伸佳侍促倣倹偶催債克免冗冠" ++
  "凍凝刑削励勘募匠匿卑卓卸厘又双吉吏哀哲啓" ++
  "喚喫嘱坑埋塊塗墜墨墳墾壇奉契奪如妨姫娯婆" ++
  "婿嫁嬢孔孤宴審寿封尿岐岳峡崩巧帆帝幻幽廉" ++
  "廊弧彫徐忌怠怪恨悔悟悦惜愚慈慌慕慨慰憂憎" ++
  "憩房抑択抽拘掃掌排掛控措掲揚換揺携搾摂撮" ++
  "擁擦敢斗斤斥施既昇晶暫架某桑棄棋楼概欧欺" ++
  "殊殴没泌浪湾湿滅滑滝滞漂漏潜潤濫瀬炉炊炎" ++
  "焦牲犠猟獄甲畔畜疾痘癖硬碑礎祉稚穂穏穫窒" ++
  "符篤簿籍粋粗粘糧紛紺絞綱緊締緩縛縫繕翻聴" ++
  "肝胆胎胞脅膜膨芳苗菊華葬藩虐虚蛮衝衰袋裂" ++
  "裸覆訂託詠該誘請諮諾謀譲豚貫賊賢赦赴超軌" ++
  "軸辛辱逮遂遇遭遵邦邪郊郭酔酵鋳錠錬錯鍛鎮" ++
  "鐘閲阻陪陳陵陶隆随隔隻雇零霊顧飽餓駐騎髄" ++
  "魂魅魔鯨鶏"

-- | The Kanji unique to Level-Pre2, considerend "mid high school" level.
preSecond :: S.Set Char
preSecond = S.fromDistinctAscList $
  "且丙亜享亭仙伯但佐併侮侯俊俸倫偏偵偽傑傘" ++
  "僕僚儒償充准凸凹刃剖剛剰劾勅勲升厄叔叙吟" ++
  "呈呉唆唇唯喝喪嗣嚇囚坪垣培堀堕堪塀塁塑塚" ++
  "塾壌壮奔奨妃妄妊妥姻娠媒嫌嫡宜宰宵寛寡寧" ++
  "寮尉尚尼履屯岬崇崎帥幣庶庸廃廷弊弔弦彰循" ++
  "徹忍恭悠患悼惰愁愉慶憤憾懇懐懲懸戻扉扶抄" ++
  "把披抹拐拒拙括拷挑挟挿捜据搭摩撤撲擬斉斎" ++
  "旋昆暁曹朕朴杉析枠枢柳栓核栽桟棚棟棺槽款" ++
  "殉殻汁江沸泡泥泰洞津洪浄浦涯涼淑渇渉渋渓" ++
  "渦溝滋漆漠漬漸潟濯煩爵猫献猶猿珠琴璽瓶甚" ++
  "畝疎疫症痢痴癒盲眺睡督矯砕硝硫碁磨礁祥禅" ++
  "禍租秩稼窃窮窯竜筒粛粧糾紡索累紳緒縄繊繭" ++
  "缶罷羅翁耗肌肖肢肯臭舶艇艦茎荘菌薦薫藻虜" ++
  "虞蚊蛇蛍融衡衷裕褐褒襟覇訟診詐詔誓諭謁謄" ++
  "謙謹譜貞貢賄賓賜賠購践軟轄迅迭逐逓逝逸遍" ++
  "遮遷還邸酌酢酪酬酷醜醸釣鈴鉢銃銘閑閥附陥" ++
  "隅雰霜靴韻頑頒頻顕飢駄騰麻"

-- | The Kanji unique to Level-2, considered "standard adult" level.
second :: S.Set Char
second = S.fromDistinctAscList $
  "串丼乞亀伎侶俺傲僅冥冶凄刹剥勃勾匂叱呂呪" ++
  "咽哺唄唾喉喩嗅嘲埼堆塞填奈妖妬媛嫉宛尻岡" ++
  "崖嵐巾弄弥彙怨恣惧慄憧憬戚戴拉拭拳拶挨挫" ++
  "捉捗捻摯斑斬旦旺昧曖曽枕柵柿栃桁梗梨椅椎" ++
  "楷毀氾汎汰沃沙淫湧溺潰煎熊爪爽牙狙玩瑠璃" ++
  "璧瓦畏畿痕痩瘍眉睦瞭瞳稽窟箋箸籠綻緻罵羞" ++
  "羨肘股脇脊腎腫腺膝膳臆臼舷艶芯苛茨萎葛蓋" ++
  "蔑蔽藍藤虎虹蜂蜜袖裾訃詣詮誰諦諧謎貌貪貼" ++
  "賂賭踪蹴辣遜遡那酎醒采釜錦錮鍋鍵鎌闇阜阪" ++
  "隙韓頃須頓頬顎餅餌駒骸鬱鶴鹿麓麺"
