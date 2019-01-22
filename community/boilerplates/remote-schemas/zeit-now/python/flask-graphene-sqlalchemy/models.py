import os
from graphene_sqlalchemy import SQLAlchemyObjectType
from sqlalchemy import Column, Integer, String, create_engine
from sqlalchemy.orm import scoped_session, sessionmaker
from sqlalchemy.ext.declarative import declarative_base

POSTGRES_CONNECTION_STRING = os.environ.get('POSTGRES_CONNECTION_STRING') or "postgres://postgres:password@localhost:6432/postgres"

engine = create_engine(POSTGRES_CONNECTION_STRING, convert_unicode=True)
db_session = scoped_session(sessionmaker(autocommit=False,
                     autoflush=False,
                     bind=engine))
Base = declarative_base()
Base.query = db_session.query_property()

class UserModel (Base):
  __tablename__ = 'users'
  id = Column(Integer, primary_key=True)
  name = Column(String)
  balance = Column(Integer)

class MinAmountModel (Base):
  __tablename__ = 'min_amount'
  amount = Column(Integer, primary_key=True)

class User(SQLAlchemyObjectType):
  class Meta:
    model = UserModel

class MinAmount(SQLAlchemyObjectType):
  class Meta:
    model = MinAmountModel
