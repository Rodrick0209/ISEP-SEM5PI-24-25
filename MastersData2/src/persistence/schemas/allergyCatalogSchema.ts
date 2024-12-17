import { IAllergyCatalogItemPersistence } from "../../dataschema/IAllergyCatalogItemPersistence";
import mongoose from 'mongoose';

const AllergyCatalogSchema = new mongoose.Schema(
    {
        domainId: { type: String, unique: true },
        code: { type: String, unique: true },
        designation: { type: String },
        description: { type: String, default: null }
    },
    {
        timestamps: true
    }
    );

export default mongoose.model<IAllergyCatalogItemPersistence & mongoose.Document>('AllergyCatalog', AllergyCatalogSchema);