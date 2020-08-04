// This file contains your Data Connector logic
section Calltouch;

[DataSource.Kind="Calltouch", Publish="Calltouch.Publish"]
shared Calltouch.Data = Value.ReplaceType(Calltouch.Func, FuncType);

FuncType = type function (
     reportType  as (type text meta [        
        Documentation.FieldCaption = "Report type:",
        Documentation.FieldDescription = "Choose the type of your report. You have 3 options: calls, requests, orders ",
         Documentation.SampleValues = {"calls"},
        Documentation.AllowedValues = { "calls","requests", "orders" }
    ]),

    ID  as (type text meta [
        Documentation.FieldCaption = "Calltouch ID:",
        Documentation.FieldDescription = "You can copy your ID from API page. Click on Integrations -> API and Webhooks",
        Documentation.SampleValues = {"12345"}
    ]),

    
    atribution  as (type text meta [
        Documentation.FieldCaption = "Attribution model:",
        Documentation.FieldDescription = "You have 2 options: Last Non-Direct (by default) and Last Click.",
        Documentation.SampleValues = {"last non-direct"},
        Documentation.AllowedValues = { "last non-direct", "last click" }
    ]),

     bindTo  as (type text meta [
        Documentation.FieldCaption = "Count by date:",
        Documentation.FieldDescription = "You have 2 options: session or call/request/order. If session was 3 days ago and call was yesterday, you can choose what date does the call belong to - 3 days ago (session) or yesterday (call/request/order).",
        Documentation.SampleValues = {"call/request/order"},
        Documentation.AllowedValues = { "call/request/order", "session" }
    ]),

    start as (type text meta [
        Documentation.FieldCaption = "Start date:",
        Documentation.FieldDescription = "Enter the first date of your statistics. You can specify date in YYYY-MM-DD format or use dynamic formats: today, yesterday, 69daysAgo.",
        Documentation.SampleValues = {"2020-02-28"}
    ]),
     end as (type text meta [
        Documentation.FieldCaption = "End date:",
        Documentation.FieldDescription = "Enter the last date of your statistics. You can specify date in YYYY-MM-DD format or use dynamic formats: today, yesterday, 69daysAgo.",
        Documentation.SampleValues = {"yesterday"}
    ])
    )
    as table meta [
        Documentation.Name = "Calltouch.Data Function",
        Documentation.LongDescription = "Created to get data from Calltouch.",
        Documentation.Examples = {
        [
            Description = "Returns a list of records from your Calltouch table",
            Code = "Calltouch.Data(""calls"",""12345"",""last non-direct"",""call/request/order"",""2020-02-20"",""yesterday"")",
            Result = "{[Record1],[Record2],[Record3],...}"
        ],
        [
            Description = "Returns a list of records from your Calltouch table",
            Code = "Calltouch.Data(""orders"",""43210"",""last click"",""session"",""500daysAgo"",""today"")",
            Result = "{[Record1],[Record2],[Record3],...}"
        ]
        }
    ];



Calltouch.Func =( reportType as text, ID as text,  atribution as text, bindTo as text, start as text, end as text)=>
let
    server = "https://api-node" & Text.From(Json.Document(Web.Contents("https://api.calltouch.ru/calls-service/RestAPI/"& ID &"/getnodeid/"))[nodeId])& ".calltouch.ru/",
    
    model = if atribution =  "last click" then "0" else "1",
    
    bind = if bindTo = "session" then [bindTo = "session"] else [],

    method = 
    if reportType="calls" then "calls-diary/calls" else 
    if reportType="requests" then "requests/" else 
    if reportType="orders" then "orders-diary/orders" else "calls-diary/calls",
    
    get0 = [#"clientApiId"=Extension.CurrentCredential()[Key], #"dateFrom"=dateFunc(start), #"dateTo"=dateFunc(end), #"limit"="1000"],
    get0Req = [#"clientApiId"=Extension.CurrentCredential()[Key], #"dateFrom"=dateFuncReq(start), #"dateTo"=dateFuncReq(end)],

    get = if reportType = "requests" then get0Req & [#"withRequestTags"="true"] else
    if reportType="calls" then get0 & [#"withCallTags"="true",attribution = model] else
    if reportType = "orders" then get0 & [#"withAllOrderOrigins"="true"] else [],

    get2= get&bind,

    web = if reportType="requests" then
    Web.Contents(server&"calls-service/RestAPI/"&method, [Query = get2, ManualStatusHandling = {404, 400}])
    else Web.Contents(server&"calls-service/RestAPI/"&ID&"/"&method, [Query = get2& [page="1"], ManualStatusHandling = {404, 400}]),
    
    json = Json.Document(web),
    pageTotal = json[pageTotal],
    
    pages =  {1..pageTotal},
    toTable = Table.FromList(pages, Splitter.SplitByNothing(), null, null, ExtraValues.Error),
    addColumn1=if Number.From(pageTotal)=0 then Table.AddColumn(toTable, "Column1", each "") else toTable,
    renameTable = Table.RenameColumns(addColumn1,{{"Column1", "page"}}),
    textType = Table.TransformColumnTypes(renameTable,{{"page", type text}}),
    dataColumn = Table.AddColumn(textType, "data", each
    Function.InvokeAfter(()=> 
        Json.Document(Web.Contents(server&"calls-service/RestAPI/"&ID&"/"&method, 
        [Query = get2& [page=[page]], ManualStatusHandling = {404, 400}]))[records],#duration(0,0,0,0.3))),
    
    expandData =  Table.ExpandListColumn(dataColumn, "data"),
    removePage = if reportType="requests" then json
    else Table.RemoveColumns(expandData,{"page"})
    in removePage;


dateFunc= (dat)=>
       if dat = "yesterday" then  Text.Replace(Date.ToText(Date.AddDays(Date.From(DateTime.LocalNow()) , -1),"dd.MM.yyyy"),".","/") 
       else if dat = "today" then Text.Replace(Date.ToText(Date.From(DateTime.LocalNow()),"dd.MM.yyyy"),".","/") 
       else if (try Number.From(Text.BeforeDelimiter(dat,"daysAgo")) otherwise 0) >0 then Text.Replace(Date.ToText(Date.AddDays(Date.From(DateTime.LocalNow()),-1*Number.From(Text.BeforeDelimiter(dat,"daysAgo"))),"dd.MM.yyyy") ,".","/") 
       else  Text.Replace(Date.ToText(Date.From(dat),"dd.MM.yyyy"),".","/") ;

dateFuncReq= (dat)=>
       if dat = "yesterday" then  Text.Replace(Date.ToText(Date.AddDays(Date.From(DateTime.LocalNow()) , -1),"MM.dd.yyyy"),".","/") 
       else if dat = "today" then Text.Replace(Date.ToText(Date.From(DateTime.LocalNow()),"MM.dd.yyyy"),".","/") 
       else if (try Number.From(Text.BeforeDelimiter(dat,"daysAgo")) otherwise 0) >0 then Text.Replace(Date.ToText(Date.AddDays(Date.From(DateTime.LocalNow()),-1*Number.From(Text.BeforeDelimiter(dat,"daysAgo"))),"MM.dd.yyyy") ,".","/") 
       else  Text.Replace(Date.ToText(Date.From(dat),"MM.dd.yyyy"),".","/") ;

Calltouch = [
    TestConnection = (dataSourcePath) => 
        { "Calltouch.Data", 
        Json.Document(dataSourcePath)[reportType], 
        Json.Document(dataSourcePath)[ID], 
        Json.Document(dataSourcePath)[atribution],
        Json.Document(dataSourcePath)[bindTo],
        Json.Document(dataSourcePath)[start],
        Json.Document(dataSourcePath)[end]},
    Authentication = [
        
         Key = []
   
    ],
    Label = "Calltouch Connection"
];



Calltouch.Publish = [
    Beta = false,
    Category = "Other",
    ButtonText = { "Calltouch Connector", "Calltouch Help" },
    LearnMoreUrl = "https://www.youtube.com/channel/UCqSLNYLN_3mf6nA1Xf7fu1g/",
    SourceImage = Calltouch.Icons,
    SourceTypeImage = Calltouch.Icons
];



Calltouch.Icons = [
    Icon16 = { Extension.Contents("calltouch16.png"), Extension.Contents("calltouch20.png"), Extension.Contents("calltouch24.png"), Extension.Contents("calltouch32.png") },
    Icon32 = { Extension.Contents("calltouch32.png"), Extension.Contents("calltouch40.png"), Extension.Contents("calltouch48.png"), Extension.Contents("calltouch64.png") }
];
