import { ComponentFixture, TestBed } from '@angular/core/testing';
import { OperationTypeComponent } from './operation-types.component';
import { ListOperationTypesComponent } from '../list-operation-types/list-operation-types.component';
import { AddOperationTypeComponent } from '../add-operation-type/add-operation-type.component';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { HttpClientTestingModule } from '@angular/common/http/testing';

describe('OperationTypeComponent', () => {
  let component: OperationTypeComponent;
  let fixture: ComponentFixture<OperationTypeComponent>;

  beforeEach(async () => {
      await TestBed.configureTestingModule({
        imports: [ListOperationTypesComponent, CommonModule, FormsModule, HttpClientTestingModule],
    }).compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(OperationTypeComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should toggle isAddingOperationType flag', () => {
    expect(component.isAddingOperationType).toBeFalse();
    component.toggleAddOperationType();
    expect(component.isAddingOperationType).toBeTrue();
    component.toggleAddOperationType();
    expect(component.isAddingOperationType).toBeFalse();
  });

  it('should set isAddingOperationType to false on form close', () => {
    component.isAddingOperationType = true;
    component.onFormClosed();
    expect(component.isAddingOperationType).toBeFalse();
  });

  it('should initialize operationType with default values', () => {
    expect(component.operationType).toEqual({ name: '' });
  });
});