import { Entity } from "../core/domain/Entity";
import { UniqueEntityID } from "../core/domain/UniqueEntityID";
import { Result } from "../core/logic/Result";
import IAllergyCathalogItemDTO from "../dto/IAllergyCatalogItemDTO";
import { AggregateRoot } from "../core/domain/AggregateRoot";



interface AllergyCatalogItemProps {
    name: string;
}


export class AllergyCathalogItem extends AggregateRoot<AllergyCatalogItemProps> {
    get id() : UniqueEntityID {
        return this._id;
    }

    get allergyId (): UniqueEntityID {
        return this.id;
    }


    get name (): string {
        return this.props.name;
    }

    private constructor (props: AllergyCatalogItemProps, id?: UniqueEntityID) {
        super(props, id);
    }

    public static create (allergyDTO: IAllergyCathalogItemDTO, id?: UniqueEntityID): Result<AllergyCathalogItem> {
        const name = allergyDTO.name;

        if (!!name === false || name.length === 0) {
            return Result.fail<AllergyCathalogItem>('Must provide a name for the allergy')
        } else {
            const allergy = new AllergyCathalogItem({ name: name }, id);
            return Result.ok<AllergyCathalogItem>( allergy )
        }
    }

}