{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
import Telegram.Bot.Simple
import Telegram.Bot.Simple.Eff
import Telegram.Bot.Simple.Reply as Rep
import Telegram.Bot.Simple.UpdateParser (updateMessageText)
import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad.IO.Class 
import System.IO
import Telegram.Bot.API.Types
import Telegram.Bot.API.Types.ReplyKeyboardMarkup  
import Telegram.Bot.API as Ap
import Data.List.Split
import Data.List 


data Model 
  = Work 
  | WaitingName ChatId                          -- Состояние для ожидания имени
  | WaitingNameFriend ChatId                    -- Состояние для ожидания имени друга для добавления
  | WaitingNameNeedFriend [String]              -- Состояние для ожидания имени друга для вывода плана
  | WaitingPlan ChatId                          -- Состояние для ожидания плана
  | WaitingDatePlan [String] ChatId             -- Состояние для ожидания даты плана
  | WaitingTimePlan [String] Text ChatId        -- Состояние для ожидания времени плана
  | WaitingPlanRemove ChatId [String] String    -- Состояние для ожидания плана, который нужно удалить
  | WaitingFriendRemove ChatId [String] String  -- Состояние для ожидания друга, которого нужно удалить

data Action 
  = StartCommand ChatId                                 -- Первая встреча и регистрация 
  | CheckingName Text ChatId                            -- Проверка имени
  | PrintName Text ChatId                               -- Регистрация нового пользователя
  | RepeatName ChatId                                   -- Запрет на регистрацию с одинаковыми именами
  | HelpFunction ChatId                                 -- Вывод всех функций 
  | TryCommand Text ChatId                              -- Запрет на регистрацию с именем команд 
  | NameFriend ChatId                                   -- Запрос имени друга для добавления
  | CheckingNameFriend Text ChatId                      -- Проверка имени друга
  | FriendNotExist ChatId                               -- Уведомление, что друга не существует
  | PrintNameFriend Text ChatId Text Text               -- Добавления человека в друзья
  | IFriend ChatId                                      -- Попытка добавить себя в друзья
  | ListFriends ChatId                                  -- Начальное взаимодействие с командой /list_friend
  | NoFriends                                           -- Уведомление, что нет друзей
  | NeedFriend [String]                                 -- Выбор друга для просмотра плана
  | NoNeedFriend [String]                               -- Уведомление, что человека нет в друзьях
  | PrintPlanFriend [String]                            -- Вывод планов друга
  | CheckingNameNeedFriend Text [String]                -- Проверка имени друга для получения плана
  | RepeatReg                                           -- Запрет на повторную регистрацию
  | RepeatFriend                                        -- Запрет на добавления друга дважды
  | Cancel                                              -- Переход в основное состояние
  | PlanFunction ChatId                                 -- Получение плана
  | DatePlan [String] ChatId                            -- Получение даты плана
  | TimePlan [String] Text ChatId                       -- Получение времени плана
  | FullPlan [String] Text Text ChatId                  -- Составление и запись плана
  | NoPlanFriend                                        -- Уведомление, что у друга нет планов
  | RemovePlan ChatId                                   -- Начальная команда для проверки команды /removal_plan
  | NoPlanUser                                          -- Уведомление об отсутствии планов у пользователя
  | ChoosePlanUser ChatId [String] String               -- Выбор плана для удаления
  | CheckingPlanRemove ChatId [String] Text String      -- Проверка плана для удаления
  | PlanNotExist ChatId [String] String                 -- Уведомление о неправильном вводе плана
  | DeletePlan                                          -- Уведомление, что план удалён
  | RemoveFriend ChatId                                 -- Начальная команда для проверки команды /removal_friend
  | ChooseFriendRemove ChatId [String] String           -- Выбор друга для удаления 
  | CheckingFriendRemove ChatId [String] Text String    -- Проверка имени друга для удаления
  | FriendRemoveNotExist ChatId [String] String         -- Уведомление о неправильном вводе друга
  | DeleteFriend                                        -- Уведомление, что друг удалён

plan_bot :: BotApp Model Action -- Главная структура бота
plan_bot = BotApp
  { 
    botInitialModel = Work      -- Начальное состояние
  , botAction = updateToAction  -- Функция для обработки обновлений
  , botHandler = all_function   -- Функция для применения действий
  , botJobs = []                
  }

alpha :: [String] -- Список основных команд
alpha = ["/start", "/plan", "/help", "/friend", "/list_friend", "/removal_plan", "/removal_friend"]


updateToAction :: Update -> Model -> Maybe Action -- Функция для обработки обновлений
updateToAction update model = case model of
        Work -> case updateMessageText update of  -- Work - основное состояние бота
            Just text -> 
                let string = Text.unpack text in
                case string of                    -- Определение полученной строки
                    "/start" -> do
                        chatId <- Ap.updateChatId update -- updateChatId - функия для определения ChatId пользователя
                        Just $ StartCommand chatId -- Обработка команды /start
                    "/help" -> do
                        chatId <- Ap.updateChatId update
                        Just $ HelpFunction chatId -- Обработка команды /help
                    "/friend" -> do
                        chatId <- Ap.updateChatId update
                        Just $ NameFriend chatId   -- Обработка команды /friend
                    "/list_friend" -> do
                        chatId <- Ap.updateChatId update
                        Just $ ListFriends chatId  -- Обработка команды /list_friend
                    "/cancel" -> do
                        Just $ Cancel              -- Обработка команды /cancel
                    "/plan" -> do
                        chatId <- Ap.updateChatId update
                        Just $ PlanFunction chatId -- Обработка команды /plan
                    "/removal_plan" -> do
                        chatId <- Ap.updateChatId update
                        Just $ RemovePlan chatId   -- Обработка команды /removal_plan
                    "/removal_friend" -> do
                        chatId <- Ap.updateChatId update
                        Just $ RemoveFriend chatId
                    _       -> Nothing             -- Если получена не команда, то ничего не происходит
            Nothing   -> Nothing                   

        WaitingName chatId -> case updateMessageText update of -- Состояние для ожидания имени
            Just name -> 
                        let string = Text.unpack name in
                            case string of 
                                "/cancel" -> do  -- Если получена команда /cancel, то переходит в основное состояние
                                    Just $ Cancel
                                _ -> Just (CheckingName name chatId) -- Функция проверки имени
            Nothing -> Nothing

        WaitingNameFriend chatId -> case updateMessageText update of -- Состояние для ожидания имени друга для добавления
            Just name ->
                        let string = Text.unpack name in
                            case string of 
                                "/cancel" -> do
                                        Just $ Cancel -- Если получена команда /cancel, то переходит в основное состояние
                                _ -> Just (CheckingNameFriend name chatId) -- Функция проверки имени друга 
            Nothing -> Nothing
        
        WaitingNameNeedFriend user_friends -> case updateMessageText update of -- Состояние для ожидания имени друга для вывода плана
            Just name_friend -> 
                        let string = Text.unpack name_friend in
                            case string of 
                                "/cancel" -> do     -- Если получена команда /cancel, то переходит в основное состояние
                                        Just $ Cancel
                                _ -> Just (CheckingNameNeedFriend name_friend user_friends) -- Функция для проверки имени
            Nothing -> Nothing
        
        WaitingPlan chatId -> case updateMessageText update of -- Состояние для ожидания плана
            Just plan -> 
                        let format_plan = words $ Text.unpack plan -- Запись плана в формате ["abc", "cde", "ryt"]
                            string = Text.unpack plan in
                            case string of
                                "/cancel" -> do     -- Если получена команда /cancel, то переходит в основное состояние
                                        Just $ Cancel
                                _ -> Just (DatePlan format_plan chatId) -- Функция для получения даты плана
            Nothing -> Nothing
        
        WaitingDatePlan plan chatId -> case updateMessageText update of  -- Состояние для ожидания даты плана
            Just date -> 
                        let string = Text.unpack date in
                            case string of
                                "/cancel" -> do    -- Если получена команда /cancel, то переходит в основное состояние
                                        Just $ Cancel
                                _ -> Just (TimePlan plan date chatId) -- Функция для получения времени плана
            Nothing -> Nothing
        
        WaitingTimePlan plan date chatId -> case updateMessageText update of -- Состояние для ожидания времени плана
            Just time -> 
                        let string = Text.unpack time in
                            case string of
                                "/cancel" -> do   -- Если получена команда /cancel, то переходит в основное состояние
                                        Just $ Cancel
                                _ -> Just (FullPlan plan date time chatId) -- Функция для записи плана 
            Nothing -> Nothing

        WaitingPlanRemove chatId plans_user name_user -> case updateMessageText update of -- Состояние для ожидания плана, который нужно удалить
            Just plan ->
                        let string = Text.unpack plan in
                            case string of 
                                "/cancel" -> do   -- Если получена команда /cancel, то переходит в основное состояние
                                        Just $ Cancel
                                _ -> Just (CheckingPlanRemove chatId plans_user plan name_user) -- Проверка плана для удаления
            Nothing -> Nothing

        WaitingFriendRemove chatId friends_user_with_id name_user -> case updateMessageText update of -- Состояние для ожидания друга, которого нужно удалить
            Just name_friend ->
                        let string = Text.unpack name_friend in
                            case string of 
                                "/cancel" -> do   -- Если получена команда /cancel, то переходит в основное состояние
                                        Just $ Cancel
                                _ -> Just (CheckingFriendRemove chatId friends_user_with_id name_friend name_user) -- Проверка имени друга для удаления
            Nothing -> Nothing

all_function :: Action -> Model -> Eff Action Model   -- Функция для применения действий
all_function action model = case action of
        StartCommand chatId -> WaitingName chatId <# do   -- Первая встреча и регистрация 
            Rep.replyText (Text.pack ("Добро пожаловать, введите свое имя!"))

        RepeatReg -> Work <# do  -- Запрет на повторную регистрацию
            Rep.replyText (Text.pack ("Вы уже зарегистрированы"))

        CheckingName name chatId -> model <# do   -- Проверка имени 
            empty_file <- liftIO $ isEmptyFile "names.txt" 
            content <- liftIO $ open_files_read "names.txt"
            let users_with_id = lines content
                all_id = [ ((words n) !! 1 ++ " " ++ (words n) !! 2) | n <- users_with_id] -- Список id всех пользователей
                id_exist = show chatId `elem` all_id 
            if empty_file -- Проверка пустоты файла 
                then if Text.unpack name `notElem` alpha -- Проверка на попытку ввода команды вместо имени
                        then if id_exist  -- Проверка зарегистрирован ли пользователь
                                then return RepeatReg -- Запрет на повторную регистрацию
                             else return $ PrintName name chatId -- Регистрация нового пользователя 
                     else return $ TryCommand name chatId -- Запрет на регистрацию с именем команд 
            else do
                let users = [ (words n) !! 0 | n <- users_with_id] -- Все зарегистрированные пользователи
                    name_exist = Text.unpack name `elem` users
                if name_exist -- Проверка существования одинакового имени
                    then return $ RepeatName chatId -- Запрет на регистрацию с одинаковыми именами
                else 
                    if Text.unpack name `notElem` alpha -- Проверка на попытку ввода команды вместо имени
                        then if id_exist -- Проверка зарегистрирован ли пользователь
                                then return RepeatReg -- Запрет на повторную регистрацию
                             else return $ PrintName name chatId -- Регистрация нового пользователя 
                    else return $ TryCommand name chatId -- Запрет на регистрацию с именем команд 

        PrintName name chatId -> Work <# do -- Регистрация нового пользователя
            liftIO $ appendFile "names.txt" $ Text.unpack name ++ " " ++ show chatId ++ "\n" -- Запись данных о пользователе
            Rep.replyText (Text.pack ("Привет, " ++ Text.unpack name ++ ", с помощью меня вы можете делиться \
                                      \планами со своими друзьями.\nМои функции:\n/start - регистрация пользователя\n\
                                      \/friend - добавить друга\n/plan - добавление плана\n\
                                      \/list_friend - показать список друзей\n/removal_plan - удаление плана\n\
                                      \/removal_friend - удаление друга\n/help - список функций\n\
                                      \/cancel - вернуться в главное меню\n\
                                      \Чем хотите воспользоваться?"))           -- Основное приветствие 

        TryCommand name chatId -> WaitingName chatId <# do  -- Запрет на регистрацию с именем команд 
            Rep.replyText (Text.pack ("Привет, " ++ Text.unpack name ++ ", \
                                      \я сомневаюсь, что ваше имя - это моя команда \
                                      \" ++ Text.unpack name ++ ", поэтому поменяйте своё имя"))

        RepeatName chatId -> WaitingName chatId <# do  -- Запрет на регистрацию с одинаковыми именами
            Rep.replyText (Text.pack ("Пользователь с таким именем уже зарегистрирован. Введите другое имя."))
        
        HelpFunction chatId -> Work <# do -- Вывод всех функций 
            Rep.replyText (Text.pack "Мои функции:\n/start - регистрация пользователя\n\
                                                  \/plan - добавление плана\n\
                                                  \/friend - добавить друга\n\
                                                  \/list_friend - показать список друзей\n\
                                                  \/removal_plan - удаление плана\n\
                                                  \/removal_friend - удаление друга\n\
                                                  \/help - список функций\n\
                                                  \/cancel - вернуться в главное меню\n\
                                                  \Чем хотите воспользоваться?")
        
        NameFriend chatId -> WaitingNameFriend chatId <# do  -- Запрос имени друга для добавления
            Rep.replyText (Text.pack "Введите имя своего друга")

        CheckingNameFriend name_friend chatId_user -> model <# do -- Проверка имени друга
            empty_file <- liftIO $ isEmptyFile "names.txt"
            if empty_file -- Проверка пустоты файла
                then return $ FriendNotExist chatId_user -- Уведомление, что друга не существует
            else do
                content <- liftIO $ open_files_read "names.txt"
                friends <- liftIO $ open_files_read "friends.txt"
                let users_with_id = lines content
                    users = [ (words n) !! 0 | n <- users_with_id] -- Имена всех пользователей 
                    name_exist = Text.unpack name_friend `elem` users
                    name_user = head [(words n) !! 0 | n <- users_with_id , ((words n) !! 1 ++ " " ++ (words n) !! 2) == show chatId_user]

                    friends_with_id = lines friends
                    user_friends = [ (words n) !! 3 | n <- friends_with_id, (words n) !! 0 == name_user] -- Все друзья пользователя
                    friend_exist = (Text.unpack name_friend) `elem` user_friends

                if name_exist  -- Проверка регистрации друга 
                    then do
                        let chatId_friend = Text.pack $ head [((words n) !! 1 ++ " " ++ (words n) !! 2) | n <- users_with_id, (words n) !! 0 == Text.unpack name_friend] -- ChatId друга
                        if Text.pack name_user == name_friend
                            then return $ IFriend chatId_user -- Попытка добавить себя в друзья 
                        else 
                            if friend_exist
                                then return RepeatFriend -- Запрет на добавления друга дважды
                            else return $ PrintNameFriend (Text.pack name_user) chatId_user name_friend chatId_friend -- Добавления человека в друзья 
                else return $ FriendNotExist chatId_user
        
        RepeatFriend -> Work <# do -- Запрет на добавления друга дважды
            Rep.replyText (Text.pack "Этот человек уже является вашим другом.")

        IFriend chatId -> WaitingNameFriend chatId <# do -- Попытка добавить себя в друзья 
            Rep.replyText (Text.pack "Вы пытаетесь добавить себя в друзья. Попробуйте еще раз.")

        FriendNotExist chatId -> WaitingNameFriend chatId <# do -- Уведомление, что друга не существует
            Rep.replyText (Text.pack "Человека с таким именем нет в базе данных. Попробуйте еще раз.")

        PrintNameFriend name_user chatId_user name_friend chatId_friend -> Work <# do -- Добавления человека в друзья 
            liftIO (appendFile "friends.txt" $ Text.unpack name_user ++ " \
                                              \" ++ show chatId_user ++ " \
                                              \" ++ Text.unpack name_friend ++ " \
                                              \" ++ Text.unpack chatId_friend ++ "\n")
            Rep.replyText (Text.pack ("Теперь " ++ Text.unpack name_friend ++ " добавлен в ваш список друзей"))
        
        NoFriends -> Work <# do  -- Уведомление, что нет друзей
            Rep.replyText (Text.pack "У вас пока что нет добавленных друзей")

        NeedFriend user_friends -> WaitingNameNeedFriend user_friends <# do -- Выбор друга для просмотра плана
            let buttons = [[KeyboardButton (Text.pack friend) Nothing Nothing Nothing Nothing Nothing Nothing] | friend <- user_friends] -- Список всех друзей ввиде кнопок
                keyboard = ReplyKeyboardMarkup
                    { replyKeyboardMarkupKeyboard = buttons
                    , replyKeyboardMarkupResizeKeyboard = Just True
                    , replyKeyboardMarkupOneTimeKeyboard = Just True
                    , replyKeyboardMarkupSelective = Nothing
                    , replyKeyboardMarkupInputFieldSelector = Nothing
                    , replyKeyboardMarkupIsPersistent = Nothing 
                    }
                replyTextParams = ReplyMessage -- Добавление клавиатуры в напечатанном сообщении
                    { replyMessageText = Text.pack "Выберите друга, планы которого вы хотите увидеть."
                    , replyMessageMessageThreadId = Nothing
                    , replyMessageParseMode  = Nothing
                    , replyMessageEntities = Nothing 
                    , replyMessageLinkPreviewOptions = Nothing
                    , replyMessageDisableNotification = Nothing
                    , replyMessageReplyToMessageId = Nothing
                    , replyMessageReplyParameters = Nothing 
                    , replyMessageReplyMarkup = Just $ SomeReplyKeyboardMarkup keyboard
                    , replyMessageProtectContent = Nothing
                    }
            reply replyTextParams

        ListFriends chatId_user -> model <# do  -- Начальное взаимодействие с командой /list_friend
            content_users <- liftIO $ open_files_read "names.txt"
            content_friends <- liftIO $ open_files_read "friends.txt"
            let users_with_id = lines content_users
                name_user = head [(words n) !! 0 | n <- users_with_id , ((words n) !! 1 ++ " " ++ (words n) !! 2) == show chatId_user]

                friends_with_id = lines content_friends
                user_friends = [ (words n) !! 3 | n <- friends_with_id, (words n) !! 0 == name_user] -- Список всех друзей пользователя
            if null user_friends -- Проверка наличия друзей 
                then return NoFriends  -- Уведомление, что нет друзей
            else do
                return $ NeedFriend user_friends -- Выбор друга для просмотра плана

        NoNeedFriend user_friends -> WaitingNameNeedFriend user_friends <# do -- Уведомление, что человека нет в друзьях
            Rep.replyText (Text.pack "Человека, с таким именем, нет в вашем списке друзей. Проверьте, правильно ли вы вводите имя.")

        PrintPlanFriend plans_friend -> Work <# do -- Вывод планов друга 
            let text = Text.pack $ "Вот список планов вашего друга:\n" ++ unlines plans_friend -- Список планов друга 
                replyTextParams = ReplyMessage  -- Удаление клавиатуры после ввода друга 
                    { replyMessageText = text 
                    , replyMessageMessageThreadId = Nothing
                    , replyMessageParseMode  = Nothing
                    , replyMessageEntities = Nothing 
                    , replyMessageLinkPreviewOptions = Nothing
                    , replyMessageDisableNotification = Nothing
                    , replyMessageReplyToMessageId = Nothing
                    , replyMessageReplyParameters = Nothing 
                    , replyMessageReplyMarkup = Just $ SomeReplyKeyboardRemove ReplyKeyboardRemove
                                                                          {
                                                                            replyKeyboardRemoveSelective = Nothing
                                                                          , replyKeyboardRemoveRemoveKeyboard = True
                                                                          }
                    , replyMessageProtectContent = Nothing
                    }
            reply replyTextParams

        CheckingNameNeedFriend name_friend user_friends -> model <# do -- Проверка имени друга для получения плана
            let friend_exist = Text.unpack name_friend `elem` user_friends
            if friend_exist -- Проверка существования друга
                then do
                    plans <- liftIO $ open_files_read "plans.txt"
                    let plans_with_names = lines plans
                        plans_friend = [ "Дело: " ++ unwords (read ((words n) !! 1)) ++ "   Дата: " ++  (words n) !! 2  ++ "   Время: " ++ (words n) !! 3  | n <- plans_with_names, (words n) !! 0 == Text.unpack name_friend]
                    if null plans_friend -- Проверка существования планов у друга
                        then return $ NoPlanFriend -- Уведомление, что у друга нет планов 
                    else return $ PrintPlanFriend plans_friend -- Вывод планов друга 
            else return $ NoNeedFriend user_friends -- Уведомление, что человека нет в друзьях
            
        NoPlanFriend -> Work <# do -- Уведомление, что у друга нет планов 
            let replyTextParams = ReplyMessage -- Отключение клавиатуры, после ввода имени друга 
                    { replyMessageText = Text.pack "У этого друга нет планов"
                    , replyMessageMessageThreadId = Nothing
                    , replyMessageParseMode  = Nothing
                    , replyMessageEntities = Nothing 
                    , replyMessageLinkPreviewOptions = Nothing
                    , replyMessageDisableNotification = Nothing
                    , replyMessageReplyToMessageId = Nothing
                    , replyMessageReplyParameters = Nothing 
                    , replyMessageReplyMarkup = Just $ SomeReplyKeyboardRemove ReplyKeyboardRemove
                                                                          {
                                                                            replyKeyboardRemoveSelective = Nothing
                                                                          , replyKeyboardRemoveRemoveKeyboard = True
                                                                          }
                    , replyMessageProtectContent = Nothing
                    }
            reply replyTextParams   

        Cancel -> Work <# do -- Переход в основное состояние
            Rep.replyText (Text.pack "Вы в главном меню.")

        PlanFunction chatId -> WaitingPlan chatId <# do -- Получение плана
            Rep.replyText (Text.pack "Напиши план, который будет виден твоим друзьям")

        DatePlan plan chatId -> WaitingDatePlan plan chatId <# do -- Получение даты плана
            Rep.replyText (Text.pack "Напиши дату в формате День.Месяц.Год")

        TimePlan plan date chatId -> WaitingTimePlan plan date chatId <# do -- Получение времени плана
            Rep.replyText (Text.pack "Напиши время в формате Часы:Минуты")
        
        FullPlan plan date time chatId -> Work <# do  -- Составление и запись плана 
            content <- liftIO $ open_files_read "names.txt"
            let users_with_id = lines content
                name_user = head [(words n) !! 0 | n <- users_with_id , ((words n) !! 1 ++ " " ++ (words n) !! 2) == show chatId]
            liftIO $ appendFile "plans.txt" (name_user ++ " " ++ show plan ++ " \
                                                \" ++ Text.unpack date ++ " \
                                                \" ++ Text.unpack time ++ "\n")
            Rep.replyText (Text.pack "Ваш план записан")

        NoPlanUser -> Work <# do -- Уведомление об отсутствии планов у пользователя
            Rep.replyText (Text.pack "У вас нет планов")

        ChoosePlanUser chatId plans_user name_user -> WaitingPlanRemove chatId plans_user name_user <# do -- Выбор плана для удаления 
            let buttons = [[KeyboardButton (Text.pack plan) Nothing Nothing Nothing Nothing Nothing Nothing] | plan <- plans_user] -- Список всех планов в виде кнопок
                keyboard = ReplyKeyboardMarkup
                    { replyKeyboardMarkupKeyboard = buttons
                    , replyKeyboardMarkupResizeKeyboard = Just True
                    , replyKeyboardMarkupOneTimeKeyboard = Just True
                    , replyKeyboardMarkupSelective = Nothing
                    , replyKeyboardMarkupInputFieldSelector = Nothing
                    , replyKeyboardMarkupIsPersistent = Nothing 
                    }
                replyTextParams = ReplyMessage -- Добавление клавиатуры в напечатанном сообщении
                    { replyMessageText = Text.pack "Выберите план, который вы хотите удалить"
                    , replyMessageMessageThreadId = Nothing
                    , replyMessageParseMode  = Nothing
                    , replyMessageEntities = Nothing 
                    , replyMessageLinkPreviewOptions = Nothing
                    , replyMessageDisableNotification = Nothing
                    , replyMessageReplyToMessageId = Nothing
                    , replyMessageReplyParameters = Nothing 
                    , replyMessageReplyMarkup = Just $ SomeReplyKeyboardMarkup keyboard
                    , replyMessageProtectContent = Nothing
                    }
            reply replyTextParams

        RemovePlan chatId -> model <# do -- Начальная команда для проверки команды /removal_plan
            content <- liftIO $ open_files_read "names.txt"
            plans <- liftIO $ open_files_read "plans.txt"
            let users_with_id = lines content
                name_user = head [(words n) !! 0 | n <- users_with_id , ((words n) !! 1 ++ " " ++ (words n) !! 2) == show chatId]
                plans_with_names = lines plans
                plans_user = [ "Дело: " ++ unwords (read ((words n) !! 1)) ++ "   Дата: " ++  (words n) !! 2  ++ "   Время: " ++ (words n) !! 3  | n <- plans_with_names, (words n) !! 0 == name_user] -- Все планы пользователя
            if null plans_user -- Проверка на наличие планов
                then return $ NoPlanUser -- Уведомление об отсутствии планов
            else return $ ChoosePlanUser chatId plans_user name_user -- Выбор плана для удаления 

        PlanNotExist chatId plans_user name_user -> WaitingPlanRemove chatId plans_user name_user <# do  -- Уведомление о неправильном вводе плана
            Rep.replyText (Text.pack "Такого плана у вас нет. Проверьте, правильно ли вы написали план")

        DeletePlan -> Work <# do -- Уведомление, что план удалён
            let replyTextParams = ReplyMessage  -- Удаление клавиатуры после ввода плана
                    { replyMessageText = Text.pack "Ваш план удалён"
                    , replyMessageMessageThreadId = Nothing
                    , replyMessageParseMode  = Nothing
                    , replyMessageEntities = Nothing 
                    , replyMessageLinkPreviewOptions = Nothing
                    , replyMessageDisableNotification = Nothing
                    , replyMessageReplyToMessageId = Nothing
                    , replyMessageReplyParameters = Nothing 
                    , replyMessageReplyMarkup = Just $ SomeReplyKeyboardRemove ReplyKeyboardRemove
                                                                          {
                                                                            replyKeyboardRemoveSelective = Nothing
                                                                          , replyKeyboardRemoveRemoveKeyboard = True
                                                                          }
                    , replyMessageProtectContent = Nothing
                    }
            reply replyTextParams

        CheckingPlanRemove chatId plans_user full_plan name_user -> model <# do -- Проверка плана для удаления
            let plan_exist = (Text.unpack full_plan) `elem` plans_user
            if plan_exist -- Проверка существования плана
                then do
                    plans <- liftIO $ open_files_read "plans.txt"
                    let all_plans = lines plans
                        format_plan = splitOn "Дата: " (Text.unpack full_plan)
                        plan = tail (words (format_plan !! 0))
                        date = head (words (format_plan !! 1))
                        time = last (words (format_plan !! 1))
                        full_string = (name_user ++ " " ++ show plan ++ " \
                                                \" ++ date ++ " \
                                                \" ++ time)  -- Строка в нужном формате, чтобы применить функцию \\
                        new_all_plans = all_plans \\ [full_string] -- Удаление нужного плана из списка всех планов
                    liftIO $ writeFile "plans.txt" (unlines new_all_plans) -- Замена файла с планами 
                    return $ DeletePlan  -- Уведомление, что план удалён
            else return $ PlanNotExist chatId plans_user name_user -- Уведомление о неправильном вводе плана

        RemoveFriend chatId -> model <# do -- Начальная команда для проверки команды /removal_friend
            content <- liftIO $ open_files_read "names.txt"
            friends <- liftIO $ open_files_read "friends.txt"
            let users_with_id = lines content
                name_user = head [(words n) !! 0 | n <- users_with_id , ((words n) !! 1 ++ " " ++ (words n) !! 2) == show chatId]
                friends_with_id = lines friends
                friends_user_with_id = [ (words n) !! 3 ++ " " ++ (words n) !! 4 ++ " " ++ (words n) !! 5  | n <- friends_with_id, (words n) !! 0 == name_user] -- Все планы пользователя
            if null friends_user_with_id -- Проверка на наличие планов
                then return $ NoFriends -- Уведомление об отсутствии друзей
            else return $ ChooseFriendRemove chatId friends_user_with_id name_user -- Выбор друга для удаления 

        ChooseFriendRemove chatId friends_user_with_id name_user -> WaitingFriendRemove chatId friends_user_with_id name_user <# do -- Выбор друга для удаления 
            let buttons = [[KeyboardButton (Text.pack ((words friend) !! 0)) Nothing Nothing Nothing Nothing Nothing Nothing] | friend <- friends_user_with_id] -- Список всех друзей в виде кнопок
                keyboard = ReplyKeyboardMarkup
                    { replyKeyboardMarkupKeyboard = buttons
                    , replyKeyboardMarkupResizeKeyboard = Just True
                    , replyKeyboardMarkupOneTimeKeyboard = Just True
                    , replyKeyboardMarkupSelective = Nothing
                    , replyKeyboardMarkupInputFieldSelector = Nothing
                    , replyKeyboardMarkupIsPersistent = Nothing 
                    }
                replyTextParams = ReplyMessage -- Добавление клавиатуры в напечатанном сообщении
                    { replyMessageText = Text.pack "Выберите друга, которого вы хотите удалить"
                    , replyMessageMessageThreadId = Nothing
                    , replyMessageParseMode  = Nothing
                    , replyMessageEntities = Nothing 
                    , replyMessageLinkPreviewOptions = Nothing
                    , replyMessageDisableNotification = Nothing
                    , replyMessageReplyToMessageId = Nothing
                    , replyMessageReplyParameters = Nothing 
                    , replyMessageReplyMarkup = Just $ SomeReplyKeyboardMarkup keyboard
                    , replyMessageProtectContent = Nothing
                    }
            reply replyTextParams

        CheckingFriendRemove chatId friends_user_with_id name_friend name_user -> model <# do -- Проверка имени друга для удаления
            let friends_user = [ (words string) !! 0 | string <- friends_user_with_id]
                friend_exist = (Text.unpack name_friend) `elem` friends_user
            if friend_exist -- Проверка существования друга
                then do
                    friends <- liftIO $ open_files_read "friends.txt"
                    let all_friends = lines friends
                        full_string = [string | string <- all_friends, (words string) !! 3 == Text.unpack name_friend] -- Строка в нужном формате, чтобы применить функцию \\
                        new_all_friends = all_friends \\ full_string -- Удаление нужного плана из списка всех планов
                    liftIO $ writeFile "friends.txt" (unlines new_all_friends) -- Замена файла с планами 
                    return $ DeleteFriend  -- Уведомление, что друг удалён
            else return $ FriendRemoveNotExist chatId friends_user_with_id name_user -- Уведомление о неправильном вводе имени друга

        FriendRemoveNotExist chatId friends_user_with_id name_user -> WaitingFriendRemove chatId friends_user_with_id name_user <# do  -- Уведомление о неправильном вводе плана
            Rep.replyText (Text.pack "Такого друга у вас нет. Проверьте, правильно ли вы написали имя друга")

        DeleteFriend -> Work <# do -- Уведомление, что друг удалён
            let replyTextParams = ReplyMessage  -- Удаление клавиатуры после ввода друга
                    { replyMessageText = Text.pack "Ваш друг удалён"
                    , replyMessageMessageThreadId = Nothing
                    , replyMessageParseMode  = Nothing
                    , replyMessageEntities = Nothing 
                    , replyMessageLinkPreviewOptions = Nothing
                    , replyMessageDisableNotification = Nothing
                    , replyMessageReplyToMessageId = Nothing
                    , replyMessageReplyParameters = Nothing 
                    , replyMessageReplyMarkup = Just $ SomeReplyKeyboardRemove ReplyKeyboardRemove
                                                                          {
                                                                            replyKeyboardRemoveSelective = Nothing
                                                                          , replyKeyboardRemoveRemoveKeyboard = True
                                                                          }
                    , replyMessageProtectContent = Nothing
                    }
            reply replyTextParams

isEmptyFile :: FilePath -> IO Bool -- Проверка файла на пустоту
isEmptyFile filePath = do
    handle <-  openFile filePath ReadMode
    isEmpty <- hIsEOF handle
    hClose handle
    return isEmpty

open_files_read :: FilePath -> IO String -- Функция для открытия файлов для чтения 
open_files_read path = do
    handle <- openFile path ReadMode
    content <- hGetContents handle
    putStrLn content
    hClose handle
    return content 

run_bot :: Token -> IO () -- Функция для запуска бота 
run_bot token = do
  env <- defaultTelegramClientEnv token -- Создание среды для бота 
  startBot_ plan_bot env  -- Запуск бота

main :: IO ()  -- Начальная функция 
main = do
    let token = Token $ Text.pack "<token>" -- Запись токена
    run_bot token -- Функция для запуска бота