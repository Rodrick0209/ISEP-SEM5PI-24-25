import { ComponentFixture, TestBed } from '@angular/core/testing';
import { OperationRequestsComponent } from './operationRequests.component';
import { ListOperationRequestsComponent } from '../list-operation-requests/list-operation-requests.component';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { OperationRequestService } from '../../services/operationRequestService';

describe('OperationRequestsComponent', () => {
  let component: OperationRequestsComponent;
  let fixture: ComponentFixture<OperationRequestsComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [OperationRequestsComponent, ListOperationRequestsComponent, HttpClientTestingModule],
      providers: [OperationRequestService]
    }).compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(OperationRequestsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should toggle isAddingOperationRequest when toggleAddOperationRequest is called', () => {
    expect(component.isAddingOperationRequest).toBeFalse();
    component.toggleAddOperationRequest();
    expect(component.isAddingOperationRequest).toBeTrue();
    component.toggleAddOperationRequest();
    expect(component.isAddingOperationRequest).toBeFalse();
  });

  it('should set isAddingOperationRequest to false when onFormClosed is called', () => {
    component.isAddingOperationRequest = true;
    component.onFormClosed();
    expect(component.isAddingOperationRequest).toBeFalse();
  });
});