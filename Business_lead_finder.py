import time
import math
import logging
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.common.action_chains import ActionChains
from telegram import Update, KeyboardButton, ReplyKeyboardMarkup
from telegram.ext import (
    ApplicationBuilder, CommandHandler, MessageHandler,
    filters, ContextTypes
)

# ========== CONFIG ==========
BOT_TOKEN = "YOUR BOTS TOKEN"
user_locations = {}  # Stores user's location by chat ID
# ============================

logging.basicConfig(level=logging.INFO)

# === TELEGRAM HANDLERS ===

async def start(update: Update, context: ContextTypes.DEFAULT_TYPE):
    location_button = KeyboardButton("📍 Send Location", request_location=True)
    reply_markup = ReplyKeyboardMarkup([[location_button]], resize_keyboard=True, one_time_keyboard=True)
    await update.message.reply_text("Hi! Tap below to share your location:", reply_markup=reply_markup)

async def location_handler(update: Update, context: ContextTypes.DEFAULT_TYPE):
    chat_id = update.effective_chat.id
    loc = update.message.location
    user_locations[chat_id] = (loc.latitude, loc.longitude)
    await update.message.reply_text("📍 Got it! Now send:\n/search <type> <radius>\nExample:\n/search coffee shops 5km")

async def search(update: Update, context: ContextTypes.DEFAULT_TYPE):
    chat_id = update.effective_chat.id

    if chat_id not in user_locations:
        await update.message.reply_text("❗ Please share your location first using /start.")
        return

    if len(context.args) < 2:
        await update.message.reply_text("Usage:\n/search <type> <radius>\nExample:\n/search cafes 3km")
        return

    query = " ".join(context.args[:-1])
    radius = context.args[-1]
    lat, lon = user_locations[chat_id]

    await update.message.reply_text(f"🔍 Searching for '{query}' within {radius}...")

    try:
        results = run_scraper(query, lat, lon)

        if not results:
            await update.message.reply_text("No businesses found.")
            return

        for business in results:
            distance_km = haversine(lat, lon, business['Lat'], business['Lon'])

            msg = (
                f"🏪 *{business['Name']}*\n"
                f"📍 {business['Address']}\n"
                f"📏 {distance_km:.1f} km away"
            )

            await update.message.reply_text(msg, parse_mode="Markdown")

    except Exception as e:
        await update.message.reply_text(f"⚠️ Error: {str(e)}")


# === SCRAPER FUNCTION ===

def run_scraper(query, lat, lon, max_results=5):
    search_url = f"https://www.google.com/maps/search/{query}/@{lat},{lon},14z"

    options = Options()
    options.add_argument("--headless")
    options.add_argument("--window-size=1920,1080")
    driver = webdriver.Chrome(options=options)
    driver.get(search_url)
    time.sleep(5)

    try:
        results_panel = driver.find_element(By.XPATH, '//div[contains(@aria-label, "Results for")]')
        for _ in range(4):
            driver.execute_script("arguments[0].scrollTop = arguments[0].scrollHeight", results_panel)
            time.sleep(1.5)
    except Exception as e:
        print("Could not scroll:", e)

    results = []
    places = driver.find_elements(By.CLASS_NAME, "hfpxzc")[:15]

    for i in range(len(places)):
        try:
            listings = driver.find_elements(By.CLASS_NAME, "hfpxzc")
            ActionChains(driver).move_to_element(listings[i]).click().perform()
            time.sleep(2)

            name = driver.find_element(By.CLASS_NAME, "DUwDvf").text

            try:
                address = driver.find_element(By.XPATH, '//button[contains(@data-item-id, "address")]').text
            except:
                address = "No address"

            try:
                website = driver.find_element(By.XPATH, '//a[contains(@data-tooltip, "Website")]').get_attribute("href")
            except:
                website = ""

            try:
                maps_url = driver.current_url
                latlon_part = maps_url.split("/@")[1].split(",")[:2]
                place_lat, place_lon = float(latlon_part[0]), float(latlon_part[1])
            except:
                place_lat, place_lon = lat, lon

            results.append({
                "Name": name,
                "Address": address,
                "Website": website,
                "Lat": place_lat,
                "Lon": place_lon
            })

        except Exception as e:
            print("Error while scraping a place:", e)

    driver.quit()
    return results[:max_results]


# === HELPER: DISTANCE CALCULATION ===

def haversine(lat1, lon1, lat2, lon2):
    R = 6371  # Earth radius in km
    phi1, phi2 = math.radians(lat1), math.radians(lat2)
    dphi = math.radians(lat2 - lat1)
    dlambda = math.radians(lon2 - lon1)

    a = math.sin(dphi/2)**2 + math.cos(phi1) * math.cos(phi2) * math.sin(dlambda/2)**2
    c = 2 * math.atan2(math.sqrt(a), math.sqrt(1 - a))

    return R * c


# === MAIN BOT LAUNCH ===

def main():
    app = ApplicationBuilder().token(BOT_TOKEN).build()
    app.add_handler(CommandHandler("start", start))
    app.add_handler(MessageHandler(filters.LOCATION, location_handler))
    app.add_handler(CommandHandler("search", search))
    app.run_polling()

if __name__ == "__main__":
    main()
