import { Entity } from "../core/domain/Entity";
import { UniqueEntityID } from "../core/domain/UniqueEntityID";
import { Result } from "../core/logic/Result";
import IAllergyDTO from "../dto/IAllergyDTO";
import { AggregateRoot } from "../core/domain/AggregateRoot";



interface AllergyProps {
    name: string;
}


export class Allergy extends AggregateRoot<AllergyProps> {
    get id() : UniqueEntityID {
        return this._id;
    }

    get allergyId (): UniqueEntityID {
        return this.id;
    }


    get name (): string {
        return this.props.name;
    }

    private constructor (props: AllergyProps, id?: UniqueEntityID) {
        super(props, id);
    }

    public static create (allergyDTO: IAllergyDTO, id?: UniqueEntityID): Result<Allergy> {
        const name = allergyDTO.name;

        if (!!name === false || name.length === 0) {
            return Result.fail<Allergy>('Must provide a name for the allergy')
        } else {
            const allergy = new Allergy({ name: name }, id);
            return Result.ok<Allergy>( allergy )
        }
    }

}