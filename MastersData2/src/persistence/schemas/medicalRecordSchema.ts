import { IMedicalRecordPersistence } from "../../dataschema/IMedicalRecordPersistence";
import mongoose from 'mongoose';
import { Allergy } from "../../domain/allergy";
import allergySchema from "./allergySchema";

const MedicalRecordSchema = new mongoose.Schema(
    {
        id: { type: String, unique: true },
        patientId: { type: String }, 
        allergies: [
            {
                id: { type: String, unique: true },
                name: { type: String, required: true },
                description: { type: String, required: true }
            }
        ],
        medicalConditions: [
            {
                id: { type: String, unique: true },
                name: { type: String, required: true },
                date: { type: Date, required: true }
            }
        ]
    },
    {
        timestamps: true
    }
);

export default mongoose.model<IMedicalRecordPersistence & mongoose.Document>('MedicalRecord', MedicalRecordSchema);