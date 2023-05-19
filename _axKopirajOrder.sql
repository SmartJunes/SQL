SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE                   Procedure [dbo].[_axKopirajOrder] (
										@cKeyPolazni			VARCHAR(13),
										@VD						char(4),
										@cZadrziBroj			Char(1) = 'N',
										@noviDat				datetime = NULL,		--16.01.18
										@noviStatus				CHAR(1) = NULL,			--16.01.18
										@nUserID				int,
										@cNewKey				VARCHAR(13) OUTPUT,
										@cErrMsg				VARCHAR(8000) OUTPUT,
										@cStatusOut				VARCHAR(1)	OUTPUT
)
AS
/*
***********************************************************************************************************
APLIKACIJA			:Pantheon
NAZIV PROCEDURE		:_axUvozPutnihNaloga_JAN
OPIS PROCEDURE	 	:Procedura za uvoz putnih naloga

ARGUMENTI			:@cKeyPolazni		- acKey dokumenta iz tHE_Order koji se kopira
					 @VD				- ciljni VD
					 @cZadrziBroj		- zadrži isti broj dokumenta na ciljnom VD-u
					 @noviDat			- datum novog dokumenta
					 @noviStatus		- status novog dokumenta, ako je NULL, prepisuje se status sa polaznog dokumenta
											(ili osnovni status ako ciljni VD nema status kakav je na polaznom dokumentu)
					 @nUserID			- ID korisnika koji poziva proceduru
					 @cNewKey			- acKey novokreiranog dokumenta
					 @cErrMsg			- poruka greške
					 @cStatusOut		- Izlazni status
					 TESSSSSSSSSSTTTTTTT TT
					  TESSSSSSSSSSTTTTTTT 1111

AUTOR				:  ZP
DATUM IZRADE		: 29.11.17.
PROMJENE				:
*************************************************************************************************************************************************************************************************************
03.01.18 - popravljeno kopiranje deviznih dokumenata
08.01.18 - dorada da pri kopiranju narudžbi napomenu sa polazne narudžbe doda na defaultnu napomenu ciljne narudžbe
16.01.18 - 2 nova argumenta, datum i status ciljnog dokumenta
18.01.18 - popravak datuma kad se spusti kroz trigger pa dođe datum=1900-01-02'
19.01.18 - iz kopiranja izbačen način nabave
08.02.18 - dorada dohvata osnovnog ugovora pri kopirnaju aneksa starog ugovora na računovodstvo
27.02.18 - podrška za dodatnu obradu '5' (kao 2 plus brisanje acFieldSE i acFieldSG) i '6' (kao 1 plus brisanje acFieldSE i acFieldSG)
14.03.18 - dodano popravljanje vrijednosnih polja u slučaju kopiranja narudžbe kupca na narudžbu dobavljača ili obrnuto jer Pantheonova procedura kopiranja pozicija ne radi ispravno
26.03.18 - dodana podrška za dodatnu obradu '7' za 0220->0270
03.04.18 - dodana nova dodatna obrada '8'
12.04.18 - podešavanje kopiranja prema zadnjem dogovoru
17.05.18 - dorada logike povezivanja dokumenata na zadnji dogovor
11.06.18 - dodana kontrola jeli osnovica za PDV na stavci ciljnog dokumenta ispravna
*************************************************************************************************************************************************************************************************************
*/

BEGIN

--insert into _zp select 			@cKeyPolazni,
--								@VD,
--								@noviDat,		--16.01.18
--								@noviStatus,			--16.01.18
--								@nUserID,
--								@cNewKey,
--								@cErrMsg,
--								@cStatusOut,
--								GETDATE()



--declare @VD char(4)='0100'
--declare @nUserID int = 1
--declare @cKeyPolazni varchar(13)='1701000000001'

declare @ddatum datetime
declare @cnovi varchar(13)=''
declare @cNovi2b varchar(13)=''
declare @p_cRokDobave varchar(1)= 'T'
declare @cTip char(1), @cSrcTip char(1), @cWayOfSale char(1)
declare @cNarocnik varchar(30), @cprejemnik varchar(30), @cskladisce varchar(30), @cOddelek varchar(30)
DECLARE @TranCount INT = @@TRANCOUNT
DECLARE @cProcName varchar(50) = '_axKopirajOrder'
DECLARE @cDodatnaAkcija VARCHAR(10)
DECLARE @cUgovor VARCHAR(35), @cRacUgovor varchar(35), @nCount int
DECLARE @LenYear INT, @LenDocType INT, @Lenbrdok INT

DECLARE @cDoc2 VARCHAR(35), @cFieldSI VARCHAR(255)

--declare @cValuta char(3)

SELECT @LenYear=anDocKeyLenYear, @LenDocType =  anDocKeyLenDocType, @Lenbrdok=anDocKeyLenNumber FROM tPA_SysParam

SET @cStatusOut='0'
SET @cErrMsg=''

IF NOT EXISTS(SELECT 1 FROM tHE_Order where ackey=@cKeyPolazni)
BEGIN
	SET @cErrMsg='Polazni dokument '+isnull(@cKeyPolazni,'NULL') +' ne postoji'
	GOTO ErrOut
END
select @cSrcTip=acType from tPA_SetDocType where acDocType=substring(@cKeyPolazni,3,4)

select @cTip=acType from tPA_SetDocType where acDocType=@VD
IF @cTip IS NULL
BEGIN
	SET @cErrMsg='Loše zadan ciljni VD:'+isnull(@VD,'NULL')
	GOTO ErrOut
END


BEGIN TRY

	BEGIN TRANSACTION
	select	@cNarocnik=CASE WHEN @cTip = @cSrcTip THEN acConsignee ELSE acReceiver END,
			@cPrejemnik=CASE WHEN @cTip = @cSrcTip THEN acReceiver ELSE acConsignee END,
			@cSkladisce = acWarehouse,
			@cOddelek = acDept,
			@cWayOfSale = acWayOfSale,
			@dDatum = adDate
			from the_order where ackey=@ckeypolazni



	if isnull(@noviDat,cast('1900-01-01' as datetime)) > '1900-01-02'		--18.01.18
		SET @ddatum = @noviDat	--18.01.18

--set @cErrMsg = '@novidat='+cast(@novidat as varchar(20))+', @dDatum='+cast(@dDatum as varchar(20))
--raiserror(@cErrMsg,16,1,'')
--return

	IF @cZadrziBroj='D'
	BEGIN
		select @cNovi2b=right(convert(varchar(4),@dDatum,120),2)+@VD+'0'+right(@cKeyPolazni,@Lenbrdok)
		if exists(select 1 from tHE_Order where ackey=@cNovi2b)
		BEGIN
			SET @cErrMsg='Dokument '+ dbo.fPA_KeyFormat(@cNovi2b,@LenYear,@LenDoctype, @Lenbrdok)+' već postoji, kopiranje nije moguće'
			GOTO ErrOut
		END
	END
	exec dbo.pHE_OrderCreAll @cPoslDog = @vd, @cNarocnik = @cNarocnik, @cPrejemnik = @cPrejemnik, @cSkladisce = @cSkladisce, @dDatum = @ddatum, @nUserId = @nUserID, @cOddelek = @cOddelek, @ckljuc = @cNovi output, @p_cKeySource = @cKeyPolazni
	IF @cZadrziBroj='D' AND @cNovi <> @cNovi2b
	BEGIN
		UPDATE tHE_Order set ackey=@cNovi2b where ackey=@cNovi
--		IF @@ERROR		-- OVAMO NEĆE NI DOĆI RADI TRY-CATCH BLOKA
--			SET @cErrMsg = ERROR_MESSAGE()+char(13)+char(10)+' pri promjeni ackey '+@cNovi+'->'+@cNovi2b
		SET @cNovi = @cNovi2b
	END

--select @cErrMsg='status:'+ISNULL(ST.acStatus,isnull(st1.osnovni,'nema'))
--		from	the_Order d
--		inner join tHE_Order o on o.ackey=@cKeyPolazni
--		LEFT JOIN tPA_SetDocTypeStat st on st.acDocType = @VD and st.acStatus=o.acStatus
--		LEFT JOIN (SELECT acDocType,MIN(acStatus) AS osnovni from tPA_SetDocTypeStat GROUP BY acDocType) st1 on st1.acDocType = @VD
--	where  d.ackey = @cNovi
--raiserror(@cErrMsg,16,1,'')
--return


	update d set
		  --[acWayOfSale] = o.acWayOfSale		--19.01.18
		  [acContactPrsn] = o.acContactPrsn
		  ,[acPriceRate] = o.acPriceRate
		  ,[acPayMethod] = o.acPayMethod
		  ,[acDelivery] = o.acDelivery
		  ,[acForm] = o.acForm
		  ,[adDeliveryDeadline] = o.adDeliveryDeadline
		  ,[anDaysForValid] = o.anDaysForValid
		  ,[adDateValid] = o.adDateValid
		  ,[anClerk] = o.anClerk
		  ,[acConfrm] = o.acConfrm
		  ,[anValue] = o.anValue
		  ,[anDiscount] = o.anDiscount
		  ,[anVAT] = o.anVAT
		  ,[anForPay] = o.anForPay
		  ,[acStatement] = o.acStatement
		  ,[acCurrency] = o.acCurrency
		  ,[anDaysForPayment] = o.anDaysForPayment
		  ,[acDept] = o.acDept
		  ,[acContactPrsn3] = o.acContactPrsn3
		  ,[acDoc1] = o.acDoc1
--		  ,[acDoc2] = o.acDoc2
		  ,[adDateDoc1] = o.adDateDoc1
		  ,[adDateDoc2] = o.adDateDoc2
		  ,[acFinished] = o.acFinished
		  ,[acParity] = o.acParity
		  ,[acParityPost] = o.acParityPost
		  ,[anBnkAcctNo] = o.anBnkAcctNo
		  ,[acCode1] = o.acCode1
		  ,[acCode2] = o.acCode2
		  ,[acCode3] = o.acCode3
		  ,[acRefNo1] = o.acRefNo1
		  ,[acRefNo2] = o.acRefNo2
		  ,[anFgnBankNo] = o.anFgnBankNo
		  ,[anNoteClerk] = o.anNoteClerk
		  ,[anRoundItem] = o.anRoundItem
		  ,[anRoundValue] = o.anRoundValue
		  ,[acRoundVATOnDoc] = o.acRoundVATOnDoc
		  ,[acRefNo3] = o.acRefNo3
		  ,[acRefNo4] = o.acRefNo4
		  ,[anSigner1] = o.anSigner1
		  ,[adSigned1] = o.adSigned1
		  ,[anSigner2] = o.anSigner2
		  ,[adSigned2] = o.adSigned2
		  ,[anSigner3] = o.anSigner3
		  ,[adSigned3] = o.adSigned3
--		  ,[acNote] = o.acNote		--08.01.18
		  ,[acNote] = CASE WHEN isnull(dt.acNote,'') <> '' THEN dt.acDefNote+CHAR(13)+CHAR(10)+dbo.fPA_RTFToANSI(o.acNote) ELSE o.acNote END --08.01.18
		  ,[acInternalNote] = o.acInternalNote
		  ,[acFieldSA] = o.acFieldSA
		  ,[acFieldSB] = o.acFieldSB
		  ,[acFieldSC] = o.acFieldSC
		  ,[acFieldSD] = o.acFieldSD
		  ,[acFieldSE] = o.acFieldSE
		  ,[acFieldSF] = o.acFieldSF
		  ,[acFieldSG] = o.acFieldSG
		  ,[acFieldSH] = o.acFieldSH
--		  ,[acFieldSI] = o.acFieldSI
		  ,[acFieldSJ] = o.acFieldSJ
		  ,[anFieldNA] = o.anFieldNA
		  ,[anFieldNB] = o.anFieldNB
		  ,[anFieldNC] = o.anFieldNC
		  ,[anFieldND] = o.anFieldND
		  ,[anFieldNE] = o.anFieldNE
		  ,[anFieldNF] = o.anFieldNF
		  ,[anFieldNG] = o.anFieldNG
		  ,[anFieldNH] = o.anFieldNH
		  ,[anFieldNI] = o.anFieldNI
		  ,[anFieldNJ] = o.anFieldNJ
		  ,[adFieldDA] = o.adFieldDA
		  ,[adFieldDB] = o.adFieldDB
		  ,[adFieldDC] = o.adFieldDC
		  ,[adFieldDD] = o.adFieldDD
		  ,[anFXRate] = o.anFXRate
		  ,[acUPNReference] = o.acUPNReference
		  ,[acUPNCode] = o.acUPNCode
		  ,[acUPNControlNum] = o.acUPNControlNum
		  ,[anCurrValue] = o.anCurrValue
		  ,[acTriangTrans] = o.acTriangTrans
		  ,[acUPNPrint] = o.acUPNPrint
		  ,[anRoundItemFC] = o.anRoundItemFC
		  ,[anRoundPrice] = o.anRoundPrice
		  ,[anRoundValueOC] = o.anRoundValueOC

		from	the_Order d
		inner join tHE_Order o on o.ackey=@cKeyPolazni
		inner join tPA_SetDocType dt on dt.acDocType = d.acDocType
	where	d.ackey = @cNovi

--	select @cValuta = acCurrency from the_order where ackey=@cKeyPolazni		--03.01.18
	select @cdoc2 = acdoc2,
			@cFieldSI = acFieldSI
		from tHE_Order where ackey=@cKeyPolazni
-----------------------------------------------------------------------------
-- dodatna kopiranja, opisana u _txParamProc
-----------------------------------------------------------------------------
	SELECT @cDodatnaAkcija=acTipObrade from _txParamProc where acProcName = @cProcName and substring(@cKeyPolazni,3,4)=acVD1 and @VD = acVD2

	IF @cDodatnaAkcija='1'				-- prepiši acDoc2 sa polaznog dokumenata u acFieldSI na ciljnom dokumentu
		BEGIN
			SELECT @cFieldSI = acdoc2 from tHE_Order  where ackey = @cKeyPolazni
			UPDATE	tHE_Order set
				acFieldSI=@cFieldSI,
				acDoc2 = @cDoc2
				 where acKey=@cNovi
		END
	ELSE IF @cDodatnaAkcija= '6'		-- prepiši acDoc2 sa polaznog dokumenata u acFieldSI na ciljnom dokumentu (kao 1) ali obriši garancije (acFieldSE, acFieldSG)	--27.02.18
		BEGIN
			SELECT @cFieldSI = acdoc2 from tHE_Order  where ackey = @cKeyPolazni
			UPDATE	tHE_Order set
				acFieldSI=@cFieldSI,
				acDoc2 = @cDoc2,
				acFieldSE='',		--27.02.18
				acFieldSG=''		--27.02.18
			 where acKey=@cNovi
		END

	ELSE IF @cDodatnaAkcija= '2'		-- prepiši interni broj polaznog dokumenta u acFieldSI
		BEGIN
			SELECT @LenYear=anDocKeyLenYear, @LenDocType =  anDocKeyLenDocType, @Lenbrdok=anDocKeyLenNumber FROM tPA_SysParam

			SELECT @cFieldSI = dbo.fPA_KeyFormat(@cKeyPolazni,@LenYear,@LenDoctype, @Lenbrdok)
			UPDATE	tHE_Order set
				acFieldSI=@cFieldSI,
				acDoc2 = @cDoc2
				 where acKey=@cNovi
		END
	ELSE IF @cDodatnaAkcija= '5'		-- prepiši interni broj polaznog dokumenta u acFieldSI (kao 2), ali obriši garancije (acFieldSE, acFieldSG)	--27.02.18
		BEGIN
			SELECT @LenYear=anDocKeyLenYear, @LenDocType =  anDocKeyLenDocType, @Lenbrdok=anDocKeyLenNumber FROM tPA_SysParam

			SELECT @cFieldSI = dbo.fPA_KeyFormat(@cKeyPolazni,@LenYear,@LenDoctype, @Lenbrdok)
			UPDATE	tHE_Order set
				acFieldSI=@cFieldSI,
				acDoc2 = @cDoc2,
				acFieldSE='',		--27.02.18
				acFieldSG=''		--27.02.18
			 where acKey=@cNovi
		END

	ELSE IF @cDodatnaAkcija= '7'		-- prepiši interni broj polaznog dokumenta u acFieldSI i acDoc2, ali obriši garancije (acFieldSE, acFieldSG)	--26.03.18
		BEGIN
			SELECT @LenYear=anDocKeyLenYear, @LenDocType =  anDocKeyLenDocType, @Lenbrdok=anDocKeyLenNumber FROM tPA_SysParam

			SELECT @cFieldSI = dbo.fPA_KeyFormat(@cKeyPolazni,@LenYear,@LenDoctype, @Lenbrdok)
			UPDATE	tHE_Order set
				acFieldSI=@cFieldSI,
				acDoc2 = @cFieldSI,
				acFieldSE='',
				acFieldSG=''
			 where acKey=@cNovi
		END

	ELSE IF @cDodatnaAkcija= '3'		-- prepiši interni broj polaznog dokumenta u acDoc2
		BEGIN
			SELECT @cFieldSI = dbo.fPA_KeyFormat(@cKeyPolazni,@LenYear,@LenDoctype, @Lenbrdok)
			UPDATE	tHE_Order set
				acFieldSI=@cFieldSI,
				acDoc2 = @cDoc2
				 where acKey=@cNovi
		END

	ELSE IF @cDodatnaAkcija= '4'		-- prepiši acFieldSI sa 0221(polaznog dokumenta) u acDoc2, interni broj osnovnog ugovora u računovodstvu (0260/0270) čiji se aneks kopira u acFieldSI'
		BEGIN
			set @nCount=0

			SELECT @nCount = @nCount + ISNULL(COUNT(*),0),
				   @cRacUgovor = dbo.fPA_KeyFormat(O260.acKey,@LenYear,@LenDoctype, @Lenbrdok)
				FROM tHE_Order Osrc
--				INNER JOIN tHE_Order O260 ON O260.acDocType='0260' AND O260.acDoc2 = OSrc.acDoc2	--08.02.18
				INNER JOIN tHE_Order O260 ON O260.acDocType='0260' AND O260.acFieldsi = OSrc.acFieldSI	--08.02.18
				WHERE Osrc.ackey = @cKeyPolazni
				group by O260.acKey

			SELECT @nCount = @nCount + ISNULL(COUNT(*),0),
				   @cRacUgovor = dbo.fPA_KeyFormat(O270.acKey,@LenYear,@LenDoctype, @Lenbrdok)
				FROM tHE_Order Osrc
				INNER JOIN tHE_Order O270 ON O270.acDocType='0270' AND O270.acDoc2 = OSrc.acFieldSI
				WHERE Osrc.ackey = @cKeyPolazni
				group by O270.acKey

			IF @cRacUgovor IS NOT NULL and @nCount = 1
			BEGIN
				SET @cFieldSI = @cRacUgovor
				UPDATE	tHE_Order set
					acFieldSI=@cFieldSI,
					acDoc2 = @cDoc2
					 where acKey=@cNovi
			END
			ELSE
			BEGIN
				SET @cErrMsg='Nije moguće odrediti interni broj osnovnog ugovora za:'+isnull(@cKeyPolazni,'NULL')
				GOTO ErrOut
			END
		END
--03.04.18
	ELSE IF @cDodatnaAkcija= '8'		-- prepiši acFieldSI sa 0221(polaznog dokumenta) u acDoc2, interni broj osnovnog ugovora u nabavi (0210/0220) čiji se aneks kopira u acFieldSI'
		BEGIN
			set @nCount=0

			SELECT @nCount = @nCount + ISNULL(COUNT(*),0),
				   @cRacUgovor = dbo.fPA_KeyFormat(O210.acKey,@LenYear,@LenDoctype, @Lenbrdok)
				FROM tHE_Order Osrc
				INNER JOIN tHE_Order O210 ON O210.acDocType='0210' AND O210.acFieldsi = OSrc.acFieldSI
				WHERE Osrc.ackey = @cKeyPolazni
				group by O210.acKey

			SELECT @nCount = @nCount + ISNULL(COUNT(*),0),
				   @cRacUgovor = dbo.fPA_KeyFormat(O220.acKey,@LenYear,@LenDoctype, @Lenbrdok)
				FROM tHE_Order Osrc
--				INNER JOIN tHE_Order O220 ON O220.acDocType='0220' AND O220.acDoc2 = OSrc.acFieldSI	--17.05.18
				INNER JOIN tHE_Order O220 ON O220.acDocType='0220' AND O220.acFieldSI = OSrc.acFieldSI	--17.05.18
				WHERE Osrc.ackey = @cKeyPolazni
				group by O220.acKey

			IF @cRacUgovor IS NOT NULL and @nCount = 1
			BEGIN
				SET @cFieldSI = @cRacUgovor
				UPDATE	tHE_Order set
					acFieldSI=@cFieldSI,
					acDoc2 = @cDoc2
					 where acKey=@cNovi
			END
			ELSE
			BEGIN
				SET @cErrMsg='Nije moguće odrediti interni broj osnovnog ugovora za:'+isnull(@cKeyPolazni,'NULL')
				GOTO ErrOut
			END

-- dohvati broj ugovora računovodstva
			set @nCount=0

			SELECT @nCount = @nCount + ISNULL(COUNT(*),0),
				   @cRacUgovor = O260.acKey
				FROM tHE_Order Osrc
				INNER JOIN tHE_Order O260 ON O260.acDocType='0260' AND O260.acFieldsi = OSrc.acFieldSI	--08.02.18
				WHERE Osrc.ackey = @cKeyPolazni
				group by O260.acKey

			SELECT @nCount = @nCount + ISNULL(COUNT(*),0),
				   @cRacUgovor = O270.acKey
				FROM tHE_Order Osrc
				INNER JOIN tHE_Order O270 ON O270.acDocType='0270' AND O270.acDoc2 = OSrc.acFieldSI
				WHERE Osrc.ackey = @cKeyPolazni
				group by O270.acKey

			IF @cRacUgovor IS NOT NULL and @nCount = 1
			BEGIN
-- poveži aneks s računovodstvenim ugovorom
			EXEC dbo._axSpojiOrderOrder
			    @p_cKeyOrder1	= @cNovi,
				@p_cKeyOrder2	= @cRacUgovor,
				@p_cType		= '_0',
				@cError			= @cErrMsg OUTPUT,
				@cOK			= @cStatusOut OUTPUT

			END
		END

	ELSE IF @cDodatnaAkcija= 'ugN->ugR'		-- prepiši acFieldSI sa (polaznog dokumenta) u acDoc2 i acFieldSI		--12.04.18
	BEGIN
		UPDATE	tHE_Order set
				acFieldSI=@cFieldSI,
				acDoc2 = @cFieldSI
			WHERE acKey=@cNovi
			SET @p_cRokDobave = 'F'
	END
	ELSE IF @cDodatnaAkcija= '->ugN'		-- prepiši formatirani acKey u acFieldSI		--12.04.18
		UPDATE	tHE_Order set
				acDoc2 = @cDoc2,	--24.05.18
				acFieldSI=dbo.fPA_KeyFormat(@cNovi,@LenYear,@LenDoctype, @Lenbrdok)
			WHERE acKey=@cNovi


	ELSE IF @cDodatnaAkcija= 'anN->anR'		-- prepiši acFieldSI sa (polaznog dokumenta) u acDoc2 i acFieldSI, poveži s ugovorom nabave i računovodstva	--12.04.18
		BEGIN
			UPDATE	tHE_Order set
					acFieldSI=@cFieldSI,
					acDoc2 = @cFieldSI
				WHERE acKey=@cNovi

			set @nCount=0
			SET @p_cRokDobave = 'F'

			SELECT @nCount = @nCount + ISNULL(COUNT(*),0),
				   @cUgovor = O210.acKey
				FROM tHE_Order Osrc
				INNER JOIN tHE_Order O210 ON O210.acDocType='0210' AND O210.acFieldsi = OSrc.acFieldSI
				WHERE Osrc.ackey = @cKeyPolazni
				group by O210.acKey

			SELECT @nCount = @nCount + ISNULL(COUNT(*),0),
				   @cUgovor = O220.acKey
				FROM tHE_Order Osrc
--				INNER JOIN tHE_Order O220 ON O220.acDocType='0220' AND O220.acDoc2 = OSrc.acFieldSI	--17.05.18
				INNER JOIN tHE_Order O220 ON O220.acDocType='0220' AND O220.acFieldSI = OSrc.acFieldSI	--17.05.18
				WHERE Osrc.ackey = @cKeyPolazni
				group by O220.acKey
			IF @cUgovor IS NOT NULL and @nCount = 1
			BEGIN
-- poveži aneks s ugovorom nabave
				EXEC dbo._axSpojiOrderOrder
					@p_cKeyOrder1	= @cNovi,
					@p_cKeyOrder2	= @cUgovor,
					@p_cType		= '_0',
					@cError			= @cErrMsg OUTPUT,
					@cOK			= @cStatusOut OUTPUT
			END
			ELSE
			BEGIN
				SET @cErrMsg='Nije moguće odrediti broj osnovnog ugovora za:'+isnull(@cKeyPolazni,'NULL')
				GOTO ErrOut
			END

-- dohvati broj ugovora računovodstva
			set @nCount=0

			SELECT @nCount = @nCount + ISNULL(COUNT(*),0),
				   @cRacUgovor = O260.acKey
				FROM tHE_Order Osrc
				INNER JOIN tHE_Order O260 ON O260.acDocType='0260' AND O260.acFieldsi = OSrc.acFieldSI	--08.02.18
				WHERE Osrc.ackey = @cKeyPolazni
				group by O260.acKey

			SELECT @nCount = @nCount + ISNULL(COUNT(*),0),
				   @cRacUgovor = O270.acKey
				FROM tHE_Order Osrc
				INNER JOIN tHE_Order O270 ON O270.acDocType='0270' AND O270.acDoc2 = OSrc.acFieldSI
				WHERE Osrc.ackey = @cKeyPolazni
				group by O270.acKey
			IF @cRacUgovor IS NOT NULL and @nCount = 1
			BEGIN
-- poveži aneks s računovodstvenim ugovorom
			EXEC dbo._axSpojiOrderOrder
			    @p_cKeyOrder1	= @cNovi,
				@p_cKeyOrder2	= @cRacUgovor,
				@p_cType		= '_0',
				@cError			= @cErrMsg OUTPUT,
				@cOK			= @cStatusOut OUTPUT

			END
		END


	ELSE
		UPDATE	tHE_Order set
			acFieldSI=@cFieldSI,
			acDoc2 = @cDoc2
				where acKey=@cNovi

-----------------------------------------------------------------------------


	DECLARE @cSkripta VARCHAR(MAX)
	if exists (select name from tempdb..sysobjects where id = object_id('tempdb..#lHE_MoveOrdAddHead'))
		drop table #lHE_MoveOrdAddHead
	CREATE TABLE #lHE_MoveOrdAddHead(_DUMMY CHAR(1))
	EXEC _axCreateTempFromParm 'lHE_MoveOrdAddHead','_DUMMY',@cSkripta OUTPUT ,@cStatusoUT OUTPUT

	if @cStatusOut='1'
	BEGIN
		SET @cErrMsg = 'Problem kod kreiranja temp tabele 1'
		goto ErrOut
	END
	EXEC (@cSkripta)
	set @cSkripta=''

	--CREATE TABLE #lHE_MoveOrdAddHead(
	--	ACKEY varchar(13),
	--	ACWAREHOUSE varchar(30),
	--	ACSUBJECT varchar(30),
	--	ACPRSN3 varchar(30),
	--	ADDATE datetime,
	--	ANFORPAY DECIMAL(19,4),
	--	ANVALUE DECIMAL(19,4),
	--	ACCONTACTPRSN varchar(30),
	--	ACDOC1 varchar(35),
	--	ACDOC2 varchar(35),
	--	ADDATEDOC1 datetime,
	--	ADDATEDOC2 datetime,
	--	ACNUMBER varchar(13),
	--	ACDOCTYPE varchar(4),
	--	ACSTATUS varchar(1),
	--	ACSETOF varchar(1),
	--	ACTYPE varchar(1),
	--	acWayOfSale varchar(1),
	--	ACMARKT varchar(1),
	--	acNote varchar(MAX),
	--	acInternalNote varchar(2000),
	--	acIsSubjectActive char(1))

	if exists (select name from tempdb..sysobjects where id = object_id('tempdb..#lHE_MoveItemInAdd'))
		drop table #lHE_MoveItemInAdd
	CREATE TABLE #lHE_MoveItemInAdd(_DUMMY CHAR(1))
	EXEC _axCreateTempFromParm 'lHE_MoveItemInAdd','_DUMMY',@cSkripta OUTPUT ,@cStatusoUT OUTPUT

	if @cStatusOut='1'
	BEGIN
		SET @cErrMsg = 'Problem kod kreiranja temp tabele 2'
		goto ErrOut
	END
	EXEC (@cSkripta)
	set @cSkripta=''

	--CREATE TABLE #lHE_MoveItemInAdd(
	--	ZapDod int,
	--	acKey char(13),
	--	anNo int,
	--	anSeqNo int,
	--	acIdent char(16),
	--	acName varchar(80),
	--	acDept char(30),
	--	acCostDrv char(16),
	--	anQty decimal(19,6),
	--	anQtyDispDoc decimal(19,6),
	--	KolicinaOrg decimal(19,6),
	--	anStockQty decimal(19,6),
	--	adDeliveryDeadline datetime,
	--	acUM char(3),
	--	acMarked char(1),
	--	anSourceItemQty decimal(19,6))

	if exists (select name from tempdb..sysobjects where id = object_id('tempdb..#lHE_SerialNoAdd'))
		drop table #lHE_SerialNoAdd
	CREATE TABLE #lHE_SerialNoAdd(_DUMMY CHAR(1))
	EXEC _axCreateTempFromParm 'lHE_SerialNoAdd','_DUMMY',@cSkripta OUTPUT ,@cStatusoUT OUTPUT

	if @cStatusOut='1'
	BEGIN
		SET @cErrMsg = 'Problem kod kreiranja temp tabele 3'
		goto ErrOut
	END
	EXEC (@cSkripta)
	set @cSkripta=''

	--CREATE TABLE #lHE_SerialNoAdd(
	--	acKey char(13),
	--	anNo int,
	--	acType char(1),
	--	acSerialNo varchar(100),
	--	acIdent char(16),
	--	acSNWOType char(2),
	--	acPackage char(3),
	--	anPackQty decimal(19,6),
	--	anQty decimal(19,6),
	--	adDateDue smalldatetime,
	--	adTimeIns datetime DEFAULT (getdate()),
	--	anUserIns int  DEFAULT ((0)),
	--	adTimeChg datetime DEFAULT (getdate()),
	--	anUserChg int default 0)

	if exists (select name from tempdb..sysobjects where id = object_id('tempdb..#lHE_MoveItemAddStat'))
		drop table #lHE_MoveItemAddStat
	CREATE TABLE #lHE_MoveItemAddStat(_DUMMY CHAR(1))
	EXEC _axCreateTempFromParm 'lHE_MoveItemAddStat','_DUMMY',@cSkripta OUTPUT ,@cStatusoUT OUTPUT

	if @cStatusOut='1'
	BEGIN
		SET @cErrMsg = 'Problem kod kreiranja temp tabele 4'
		goto ErrOut
	END
	EXEC (@cSkripta)
	set @cSkripta=''

	--CREATE TABLE #lHE_MoveItemAddStat(
	--	acKey char(13),
	--	anNo int,
	--	acKeyAdd char(13),
	--	anNoAdd int,
	--	acIdent char(16),
	--	acName varchar(80),
	--	anPlanQty decimal(19,6),
	--	PreKolicina decimal(19,6),
	--	acUM char(3),
	--	acError char(3),
	--	acComment varchar(2000),
	--	acTransferType char(1))

	if exists (select name from tempdb..sysobjects where id = object_id('tempdb..#lHE_OrderAdd01'))
		drop table #lHE_OrderAdd01
	CREATE TABLE #lHE_OrderAdd01(_DUMMY CHAR(1))
	EXEC _axCreateTempFromParm 'lHE_OrderAdd01','_DUMMY',@cSkripta OUTPUT ,@cStatusoUT OUTPUT

	if @cStatusOut='1'
	BEGIN
		SET @cErrMsg = 'Problem kod kreiranja temp tabele 5'
		goto ErrOut
	END
	EXEC (@cSkripta)
	set @cSkripta=''

	--CREATE TABLE #lHE_OrderAdd01(
	--	acSetOfItem char(3),
	--	acKey char(13))

	if exists (select name from tempdb..sysobjects where id = object_id('tempdb..#LHE_MOVEITEMINADD_CRPOZ'))
		drop table #LHE_MOVEITEMINADD_CRPOZ
	CREATE TABLE #LHE_MOVEITEMINADD_CRPOZ(_DUMMY CHAR(1))
	EXEC _axCreateTempFromParm 'LHE_MOVEITEMINADD_CRPOZ','_DUMMY',@cSkripta OUTPUT ,@cStatusoUT OUTPUT

	if @cStatusOut='1'
	BEGIN
		SET @cErrMsg = 'Problem kod kreiranja temp tabele 6'
		goto ErrOut
	END
	EXEC (@cSkripta)
	set @cSkripta=''


	--CREATE TABLE #LHE_MOVEITEMINADD_CRPOZ(
	--	ACKEY VARCHAR(13),
	--	ANNO INT,
	--	ACIDENT VARCHAR(16),
	--	ACNAME VARCHAR(80),
	--	ANQTY float,
	--	ACUM VARCHAR(3),
	--	ANPRICE FLOAT,
	--	ANREBATE FLOAT,
	--	ANREBATE1 FLOAT,
	--	ANREBATE2 FLOAT,
	--	ANREBATE3 FLOAT,
	--	ACVATCODE VARCHAR(2),
	--	ANEXCISE FLOAT,
	--	ANEXCISEP FLOAT,
	--	ANVAT FLOAT,
	--	ACNOTE VARCHAR(2000),
	--	ANSALEPRICE DECIMAL,
	--	ACNAMEITEM VARCHAR(80),
	--	ACUMITEM VARCHAR(3),
	--	ACDEPT VARCHAR(30),
	--	ACCOSTDRV VARCHAR(16),
	--	ACTYPE VARCHAR(1),
	--	ADDELIVERYDEADLINE DATETIME,
	--	ANSEQNO INT,
	--	ANPACKQTY decimal(19,6),
	--	ANDIMWEIGHT FLOAT,
	--	ANDIMWEIGHTBRUTTO FLOAT,
	--	ANDIMVOLUME FLOAT,
	--	ANDIMWEIGHTITEM decimal(19,6),
	--	ANDIMWEIGHTBRUTTOITEM decimal(19,6),
	--	ANDIMVOL decimal(19,6),
	--	ACCLASSIF VARCHAR(16),
	--	ACORIGIN VARCHAR(3),
	--	ANSOURCEITEMQTY float,
	--	ACUM2 char(3))

	if exists (select name from tempdb..sysobjects where id = object_id('tempdb..#LHE_MOVEITEMINADD_CRKOS'))
		drop table #LHE_MOVEITEMINADD_CRKOS
	CREATE TABLE #LHE_MOVEITEMINADD_CRKOS(_DUMMY CHAR(1))
	EXEC _axCreateTempFromParm 'LHE_MOVEITEMINADD_CRKOS','_DUMMY',@cSkripta OUTPUT ,@cStatusoUT OUTPUT

	if @cStatusOut='1'
	BEGIN
		SET @cErrMsg = 'Problem kod kreiranja temp tabele 7'
		goto ErrOut
	END
	EXEC (@cSkripta)
	set @cSkripta=''


	--CREATE TABLE #LHE_MOVEITEMINADD_CRKOS(
	--	ACIDENT VARCHAR(16),
	--	ANSUBNO INT,
	--	ACIDENTCHILD VARCHAR(16),
	--	ACNAME VARCHAR(80),
	--	ANQTY FLOAT,
	--	ACUM VARCHAR(3),
	--	ANPRICE FLOAT,
	--	ANREBATE FLOAT,
	--	ACNOTE VARCHAR(255),
	--	ANSALEPRICE DECIMAL,
	--	ANWASTEQTY NUMERIC(19,6),
	--	ACSETOFITEM VARCHAR(3),
	--	ACNAMEITEM VARCHAR(80),
	--	ACUMITEM VARCHAR(3))

	if exists (select name from tempdb..sysobjects where id = object_id('tempdb..#LHE_MOVEITEMINADD_CRLINKS'))
		drop table #LHE_MOVEITEMINADD_CRLINKS
	CREATE TABLE #LHE_MOVEITEMINADD_CRLINKS(_DUMMY CHAR(1))
	EXEC _axCreateTempFromParm 'LHE_MOVEITEMINADD_CRLINKS','_DUMMY',@cSkripta OUTPUT ,@cStatusoUT OUTPUT

	if @cStatusOut='1'
	BEGIN
		SET @cErrMsg = 'Problem kod kreiranja temp tabele 8'
		goto ErrOut
	END
	EXEC (@cSkripta)
	set @cSkripta=''

--------------------------------------------------
--------------------------------------------------
		if exists (select name from tempdb..sysobjects where id = object_id('tempdb..#lHE_OrderDDV'))
		drop table #lHE_OrderDDV
	CREATE TABLE #lHE_OrderDDV(_DUMMY CHAR(1))
	EXEC _axCreateTempFromParm 'lHE_OrderDDV','_DUMMY',@cSkripta OUTPUT ,@cStatusoUT OUTPUT

	if @cStatusOut='1'
	BEGIN
		SET @cErrMsg = 'Problem kod kreiranja temp tabele 9'
		goto ErrOut
	END
	EXEC (@cSkripta)
	set @cSkripta=''

	--CREATE TABLE #LHE_MOVEITEMINADD_CRLINKS(
	--	acKey varchar(13),
	--	anNo int,
	--	acIdent varchar(16),
	--	anQty numeric(19,6),
	--	anPrice float,
	--	anRebate float,
	--	acDept varchar(30),
	--	acCostDrv varchar(16),
	--	acOrigin varchar(3))


	insert into #lHE_MoveOrdAddHead (acKey, acWareHouse, acSubject, acPrsn3, adDate, anForPay,
									 anValue, acContactPrsn, acDoc1, acDoc2, adDateDoc1, adDateDoc2,
									 acNumber, acDocType, acStatus, acSetOf, acType, acWayOfSale, acMarkt, acNote, acInternalNote)
							select	acKey, acWareHouse, acSubject, acPrsn3, adDate, anForPay,
									anValue, acContactPrsn, acDoc1, acDoc2, adDateDoc1, adDateDoc2,
									acNumber, o.acDocType, ISNULL(ST.acStatus,st1.osnovni), acSetOf, acType, acWayOfSale, 'T', acNote, acInternalNote
							from vHE_AddOrderLookupOrd o
							LEFT JOIN tPA_SetDocTypeStat st on st.acDocType = @VD and st.acStatus=o.acStatus
							LEFT JOIN (SELECT acDocType,MIN(acStatus) AS osnovni from tPA_SetDocTypeStat GROUP BY acDocType) st1 on st1.acDocType = @VD
							where acKey = @cKeyPolazni

	declare @cFilter varchar(2000)=' and P.acKey = '''+@cKeyPolazni+''''


	exec pHE_AddingFillTempOrder
			@p_lDodajOdpremnaLista = 'F'
			,@p_cField = ''
			,@p_cSklad = ''
			,@p_fPrikVsePoz = 'T'
			,@p_cFilter = @cFilter
			,@p_cDataBase = ''
			,@p_cKeyHead = @cKeyPolazni
			,@p_cDocType = @vd
			,@p_dDate = @ddatum
			,@p_cTransferWareHouse = 'F'


		EXEC pHE_AddingMoveOrder2Order
			@p_iType = '1',
			@p_cKljucIz = @cKeyPolazni,
			@p_cValutaIz = '',--@cValuta,--HRK
			@p_cKeyList ='',
			@p_cKljucNa = @cNovi,
			@p_cValutaNa = '',--@cValuta, -- HRK
			@p_cadDate = @dDatum,
			@p_cPartner ='',
			@p_cacWarehouse =@cskladisce,
			@p_cacPriceRate = '',--1
			@p_cacWayOfSale = @cWayOfSale,
			@p_dHeadDeliveryDeadLine ='', --ako je zadan i ne prenosi se rok isporuke s polaznog dokumenta, upisuje se u rok isporuke na pozicije
			@p_cacType = @cTip,	--=tHE_SetDocType.acType na ciljnom VD-u
			@p_cCopyTip = 'N',--N:kopiranje (linkova) Order-Order, =T:kopiranje (linkova) Order-Move
			@p_cNarocNeVrstaMS = '',
			@p_cIsciNaziv = 'F',
			@p_cIsciDim = 'F',
			@p_cIsciCene = 'F',
			@p_cIsciTS = 'F',
			@p_cOnlyActive = 'F',
			@p_cIsCurrentActive = 'T',
			@p_cPrenos = '',--0		-- prenijeti i veze među dokumentima
			@p_cPredZnak = 'F',	--='T'-> kolicina = -1*kolicina
			@p_cacDatabaseName = '',
			@p_cFilterOznacen = ' and T.acMarked = ''T'' ',
			@p_cacTableName = 'tHE_OrderItem',
			@p_cfIsciNaziv = 'F',			--T:naziv iz tHE_SetItem
			@p_cRokDobave =  @p_cRokDobave ,	-- prenesi rok isporuke sa polaznog dokumenta
			@p_cStockIdents = 'F',	--T:samo identi sa zalihom na dan
			@p_cTransferCourse = 'F',--'T',		-- prenesi tečaj sa polaznog dokumenta	-- 03.01.18
			@p_nUserId = @nUserID,
			@p_cConfTransDoc = 'F',
			@p_cTransferPosNote = 'T'

	--select '#LHE_MOVEITEMINADD_CRPOZ',* from #LHE_MOVEITEMINADD_CRPOZ
			if exists(select top 1 acCurrency from the_order where acCurrency ='HRK' and ackey =@cNovi) --and year(adDateDoc1)=2023 and year(addate)=2023
			begin
				update the_Order SET acCurrency  ='EUR' where acCurrency ='HRK' and ackey =@cNovi
				update the_Order SET anFxRate  ='1.000000' where ackey =@cNovi
				update the_OrderItem set anprice = anprice/7.53450 where ackey =@cNovi
			end

-- 14.03.18	- Preračunaj iznose na pozicijama ako se kopira sa ulazne narudžbe na izlaznu i obrnuto
--	IF @cTip <> @cSrcTip	----12.06.18
	BEGIN
		DECLARE crPoz CURSOR LOCAL FAST_FORWARD FOR SELECT anNo FROM tHE_OrderItem where ackey=@cNovi
		DECLARE @nPoz INT
		OPEN crPoz
		FETCH NEXT FROM crPoz into @nPoz
		WHILE @@FETCH_STATUS=0
		BEGIN
			IF @cTIP='I'
				EXEC pHE_OrderItemInPVItem @cNovi, @nPoz
			ELSE
				EXEC pHE_OrderItemOutPVItem @cNovi, @nPoz
			FETCH NEXT FROM crPoz into @nPoz
		END
		CLOSE crPoz
		DEALLOCATE crPOZ
	END
--14.03.18 kraj

		EXEC pHE_OrderSetSum
			@cOsnZaIzrProdCene = '',
			@cValuta = '',
			@cNasaValuta = '',
			@nRoundT = 0,
			@nRoundVR = 0,
			@cKljuc = @cNovi,
			@nUserId = @nUserID

	update d set 			-- prepiši status sa polazne narudžbe
--		  acStatus = CASE WHEN @noviStatus IS NULL THEN ISNULL(ST.acStatus,isnull(st1.osnovni,'')) ELSE @noviStatus END
		  acStatus = ISNULL(ST.acStatus,isnull(st1.osnovni,''))
		from	the_Order d
		inner join tHE_Order o on o.ackey=@cKeyPolazni
		LEFT JOIN tPA_SetDocTypeStat st on st.acDocType = @VD and st.acStatus=isnull(@noviStatus,o.acStatus)
		LEFT JOIN (SELECT acDocType,MIN(acStatus) AS osnovni from tPA_SetDocTypeStat GROUP BY acDocType) st1 on st1.acDocType = @VD
	where  d.ackey = @cNovi

	EXEC _axSpojiOrderOrder @cKeyPolazni, @cNovi,  '_0', @cErrMsg, @cStatusOut


-- 11.06.18 START
	IF EXISTS(	SELECT 1
		from tHE_OrderItem OI
		inner join the_order o on o.ackey = OI.ackey
		where OI.acKey=@cNovi and OI.anPVVATBase =0	and OI.anPrice != 0 and OI.anvat<>0 and OI.anQty<>0)
	BEGIN
		SET @cErrMsg = 'Osnovica za PDV nije definirana:'
		SELECT @cErrMsg = @cErrMsg + OI.acKey+'/'+cast(OI.anNo as varchar(10))
			from tHE_OrderItem OI
			inner join the_order o on o.ackey = OI.ackey
			where OI.acKey=@cNovi and OI.anPVVATBase =0	and OI.anPrice != 0 and OI.anvat<>0 and OI.anQty<>0
		GOTO ErrOut
	END
-- 11.06.18 END

	if 	@p_cRokDobave = 'F'
	BEGIN
		UPDATE tHE_OrderItem SET adDeliverydeadline = NULL WHERE acKey = @cNovi
	END

	COMMIT TRANSACTION
	set @cNewKey = @cnovi
	set @cStatusOut='0'
	set @cErrMsg=''


	RETURN
END TRY

BEGIN CATCH
	set @cErrMsg = ERROR_MESSAGE()
END CATCH
ErrOut:
	SET @cStatusOut = '1'
	set @cNewKey = NULL
	SET @cErrMsg = isnull(@cErrMsg,'null') + char(13)+char(10)+'('+@cProcName+')'
	if @@TRANCOUNT > @TranCount
		ROLLBACK TRANSACTION
	RAISERROR(@cErrMsg,16,1,'')
END
GO
