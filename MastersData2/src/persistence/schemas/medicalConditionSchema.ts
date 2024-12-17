import { IMedicalConditionPersistence } from "../../dataschema/IMedicalConditionPersistence";
import mongoose from 'mongoose';

const MedicalConditionSchema = new mongoose.Schema(
    {
        domainId: { type: String, unique: true },
        medicalConditionCatalog: { type: String},
        description: { type: String }
    },
    {
        timestamps: true
    }
    );

export default mongoose.model<IMedicalConditionPersistence & mongoose.Document>('MedicalCondition', MedicalConditionSchema);