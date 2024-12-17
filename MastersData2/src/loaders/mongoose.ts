import mongoose from 'mongoose';
import { Db } from 'mongodb';
import config from '../../config';

export default async (): Promise<Db> => {
  const connection = await mongoose.connect(config.databaseURL);
  const db = connection.connection.db;

  /*const collections = await db.listCollections().toArray();

  try {
    const collections = await db.listCollections().toArray();
    collections.map(async (collection) => {
      const coll = db.collection(collection.name);
      await coll.dropIndexes();

      console.log(`Indexes dropped successfully for collection: ${collection.name}`);
    });
  } catch (error) {
    console.error('Error dropping indexes:', error);
  }*/

  return db;
};
