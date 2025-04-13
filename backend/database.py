
from sqlalchemy import create_engine
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import sessionmaker
from sqlalchemy.ext.asyncio import create_async_engine, AsyncSession
import os

# Database URL
DATABASE_URL = os.environ.get("DATABASE_URL", "sqlite+aiosqlite:///./cobol_whisperer.db")
SYNC_DATABASE_URL = DATABASE_URL.replace("+aiosqlite", "")

# Create async engine
engine = create_async_engine(
    DATABASE_URL, connect_args={"check_same_thread": False} if "sqlite" in DATABASE_URL else {}
)
SessionLocal = sessionmaker(engine, class_=AsyncSession, expire_on_commit=False)

# Sync engine for migrations
sync_engine = create_engine(SYNC_DATABASE_URL)

# Base class for models
Base = declarative_base()

# Dependency to get database session
async def get_db():
    db = SessionLocal()
    try:
        yield db
    finally:
        await db.close()
