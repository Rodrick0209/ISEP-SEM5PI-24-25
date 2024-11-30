import { AggregateRoot } from "../core/domain/AggregateRoot";
import { UniqueEntityID } from "../core/domain/UniqueEntityID";
import { Result } from "../core/logic/Result";
import { MedicalRecordId } from "./medicalRecordId";
import { MedicalCondition } from "./medicalCondition";
import { Allergy} from "./allergy";
import  IMedicalRecordDTO  from "../dto/IMedicalRecordDTO";



interface MedicalRecordProps {
    patientId: string;
    allergies: Allergy[];
    medicalConditions: MedicalCondition[];
  }

export class MedicalRecord extends AggregateRoot<MedicalRecordProps> {
    get id(): UniqueEntityID {
        return this._id;
    }

    get roleId (): MedicalRecordId {
        return new MedicalRecordId(this.id.toValue());
    }

    get patientId (): string {
        return this.props.patientId;
    }

    get allergies (): Allergy[] {
        return this.props.allergies;
    }

    get medicalConditions (): MedicalCondition[] {
        return this.props.medicalConditions;
    }

    private constructor (props: MedicalRecordProps, id?: UniqueEntityID) {
        super(props, id);
    }

    public static create (IMedicalRecordDTO: IMedicalRecordDTO, id?: UniqueEntityID): Result<MedicalRecord> {
        const allergies = IMedicalRecordDTO.allergies;
        const medicalConditions = IMedicalRecordDTO.medicalConditions;

        const medicalRecord = new MedicalRecord({ patientId: IMedicalRecordDTO.patientId, allergies: allergies, medicalConditions: medicalConditions }, id);
        return Result.ok<MedicalRecord>( medicalRecord )
    }




}
